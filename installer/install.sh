#!/usr/bin/env bash

# Consolidated NixOS provisioning script.
#
# Handles three machine types:
#   - Personal computer (bare metal + LUKS + ZFS + user env setup)
#   - Bare metal server  (ZFS, no LUKS)
#   - Virtual machine     (ext4 + LVM)
#
# References:
# https://github.com/nix-community/nixos-anywhere/blob/main/docs/howtos/secrets.md
# https://gist.github.com/jkachmar/7305140b55ff1297703a2d2218964e54
# https://github.com/jkachmar/termina/blob/main/hosts/tatl/disks.nix
# https://nixos.wiki/wiki/Yubikey_based_Full_Disk_Encryption_(FDE)_on_NixOS#Partitioning

set -e

# shellcheck source=lib/truenas.sh
source "${INSTALLER_LIB_DIR:-installer/lib}/truenas.sh"

cleanup() {
  rm -rf "$temp" "$ssh_temp"
}

# Print the UniFi steps for a fixed IP reservation.
print_unifi_steps() {
    local machine="$1" mac="$2"
    cat >&2 <<EOF

Make a fixed IP reservation for this VM in UniFi:
  MAC address: ${mac}
  1. Open the UniFi Network web UI.
  2. Go to Settings > Networks, and note the DHCP range of the LAN.
  3. Go to Client Devices. The VM is not on the network yet, so choose
     "Add client" (or use Settings > Networks > LAN > DHCP > Fixed IP).
  4. Enter the MAC address above and the name ${machine}.
  5. Turn on "Fixed IP Address" and enter a free address.
  6. Save.
When the reservation is saved, enter the IP address here.

EOF
}

# Find the installer ISO file. Build it if the wrapper did not give one.
find_installer_iso() {
    local out="${INSTALLER_ISO:-}" iso
    if [ -z "${out}" ]; then
        out=$(nix build '.#nixos-iso' --no-link --print-out-paths) || return 1
    fi
    if [ -z "${out}" ] || [ ! -d "${out}" ]; then
        return 1
    fi
    # The store directory name also ends in .iso, so match files only.
    iso=$(find "${out}/" -type f -name '*.iso' -print -quit)
    if [ -z "${iso}" ]; then
        return 1
    fi
    echo "${iso}"
}

# Remove the ISO from the VM. A CD-ROM that stays attached does no harm,
# because the disk comes first in the boot order. So a failure here is
# only a warning.
detach_installer_iso() {
    if truenas_detach_iso "$1"; then
        gum log --level info "Removed the installer ISO from the VM."
    else
        gum log --level warn "Remove the CD-ROM device by hand: Virtual Machines > the VM > Devices, in the TrueNAS web UI."
    fi
}

# Create the VM on TrueNAS, or reuse one from an earlier run. Set IP and
# TRUENAS_VM_ID. Return 2 if Solomon does not want a TrueNAS VM.
install_truenas_vm() {
    local machine="$1" vm_name check mac cpus ram disk vnc_password iso iso_file waited

    truenas_load_key || exit 1
    vm_name=$(truenas_vm_name "${machine}")
    check=$(truenas_check_vm "${vm_name}") || exit 1

    case "${check}" in
        foreign)
            gum log --level error "A VM named ${vm_name} exists on TrueNAS, but the installer did not make it. The installer will not change it."
            exit 1
            ;;
        absent)
            if ! gum confirm "Create this VM on TrueNAS?"; then
                return 2
            fi
            if truenas_zvol_exists "${machine}"; then
                gum log --level error "The zvol ${TRUENAS_POOL}/${machine} exists already. The installer will not change it."
                exit 1
            fi
            while true; do
                cpus=$(gum input --value 2 --header "Guest CPUs (even number, 2 to 16)")
                ram=$(gum input --value 4 --header "RAM in GiB")
                disk=$(gum input --value 50 --header "Disk size in GiB")
                if truenas_validate_specs "${cpus}" "${ram}" "${disk}"; then
                    break
                fi
            done

            mac=$(truenas_gen_mac)
            vnc_password=$(truenas_gen_vnc_password)
            # The VM does not exist, so an old entry here is from a failed run.
            if ! echo "${vnc_password}" | pass insert -m -f "machine/${machine}/vnc-password" >/dev/null; then
                gum log --level error "Cannot store the VNC password in pass at machine/${machine}/vnc-password."
                exit 1
            fi
            VNC_PASSWORD_MADE=1

            if ! iso_file=$(find_installer_iso); then
                gum log --level error "Cannot build or find the installer ISO (nix build '.#nixos-iso')."
                exit 1
            fi
            iso=$(truenas_ensure_iso "${iso_file}") || exit 1
            TRUENAS_VM_ID=$(truenas_create_vm "${machine}" "${cpus}" "${ram}" "${disk}" "${mac}" "${iso}" "${vnc_password}") || exit 1
            gum log --level info "Created the VM ${vm_name} on TrueNAS (id ${TRUENAS_VM_ID})"
            ;;
        ours\ *)
            gum log --level info "Found the VM ${machine} from an earlier run. The installer will use it."
            TRUENAS_VM_ID=$(jq -r '.id' <<<"${check#ours }")
            mac=$(truenas_vm_mac "${check#ours }")
            ;;
    esac

    print_unifi_steps "${machine}" "${mac}"
    IP=$(gum input --placeholder "reserved IP address...")
    if [ -z "${IP}" ]; then
        gum log --level error "IP address cannot be empty."
        exit 1
    fi

    if [ "$(truenas_vm_state "${TRUENAS_VM_ID}")" != "RUNNING" ]; then
        truenas_start_vm "${TRUENAS_VM_ID}" || exit 1
    fi

    # The ISO makes new host keys at each boot. Remove old keys for this
    # IP, and accept the first key that the ISO shows. The later steps of
    # the installer then use the normal known_hosts file.
    ssh-keygen -R "${IP}" >/dev/null 2>&1 || true
    gum log --level info "Waiting for the VM to answer on SSH at ${IP}..."
    waited=0
    until ssh -o ConnectTimeout=5 -o BatchMode=yes -o StrictHostKeyChecking=accept-new "root@${IP}" true 2>/dev/null; do
        if [ "${waited}" -ge "${TRUENAS_SSH_TIMEOUT:-300}" ]; then
            gum log --level error "The VM did not answer on SSH at ${IP}. Look at the VM console in the TrueNAS web UI. Run the installer again to continue with this VM."
            exit 1
        fi
        sleep "${TRUENAS_SSH_INTERVAL:-5}"
        waited=$((waited + ${TRUENAS_SSH_INTERVAL:-5}))
    done
}

main() {
    temp=""
    ssh_temp=""
    trap cleanup EXIT

    # ── 1. Machine type ──────────────────────────────────────────────
    gum log --level info "What kind of machine?"
    MACHINE_KIND=$(gum choose "personal computer" "server")

    if [ "$MACHINE_KIND" = "personal computer" ]; then
        MACHINE_DIR="personal-computers"
        DEFAULT_TEMPLATE="installer/templates/pc-default.nix"
        DISK_TEMPLATE="installer/templates/zfs-luks-disk-config.nix"
        IS_PC=1
    else
        MACHINE_DIR="servers"
        IS_PC=0

        gum log --level info "What type of server?"
        SERVER_KIND=$(gum choose "bare metal" "virtual machine")

        if [ "$SERVER_KIND" = "bare metal" ]; then
            DEFAULT_TEMPLATE="installer/templates/bare-metal-server-default.nix"
            DISK_TEMPLATE="installer/templates/zfs-disk-config.nix"
        else
            DEFAULT_TEMPLATE="installer/templates/vm-server-default.nix"
            DISK_TEMPLATE="installer/templates/lvm-disk-config.nix"
        fi
    fi

    MACHINE_PATH="config/machines/${MACHINE_DIR}"
    mkdir -p "${MACHINE_PATH}"

    # ── 2. Pick or create machine ───────────────────────────────────
    gum log --level info "Pick a machine:"
    MACHINE=$({ find "${MACHINE_PATH}" -maxdepth 1 -mindepth 1 -type d -printf '%f\n' | sort; echo "+ create new"; } | gum choose)

    if [ "$MACHINE" = "+ create new" ]; then
        MACHINE=$(gum input --placeholder "machine name...")
        if [ -z "$MACHINE" ]; then
            gum log --level error "Machine name cannot be empty."
            exit 1
        fi
        if [[ ! "$MACHINE" =~ ^[a-z0-9]([a-z0-9-]*[a-z0-9])?$ ]]; then
            gum log --level error "Machine name must be lowercase alphanumeric with hyphens (e.g. 'my-server-1')."
            exit 1
        fi
        mkdir -p "${MACHINE_PATH}/${MACHINE}"
        gum log --level info "Created ${MACHINE_PATH}/${MACHINE}"
    fi

    TARGET="${MACHINE_PATH}/${MACHINE}"

    # ── 3. TrueNAS VM and IP address ─────────────────────────────────
    TRUENAS_VM_ID=""
    VNC_PASSWORD_MADE=0
    TRUENAS_STATUS=2
    if [ "$IS_PC" -eq 0 ] && [ "$SERVER_KIND" = "virtual machine" ]; then
        install_truenas_vm "$MACHINE" && TRUENAS_STATUS=0 || TRUENAS_STATUS=$?
    fi

    if [ "$TRUENAS_STATUS" -eq 2 ]; then
        IP=$(gum input --placeholder "enter address..")
        if [ -z "$IP" ]; then
            gum log --level error "IP address cannot be empty."
            exit 1
        fi
    fi

    # ── 4. Pre-flight: LUKS key and user SSH keys (PCs only) ────────
    if [ "$IS_PC" -eq 1 ]; then
        if ! pass show "machine/${MACHINE}/luks/key/0" >/dev/null 2>&1; then
            gum log --level info "No LUKS password found for ${MACHINE}, creating one..."
            LUKS_PASS=$(gum input --password --placeholder "enter LUKS password...")
            if [ -z "$LUKS_PASS" ]; then
                gum log --level error "LUKS password cannot be empty."
                exit 1
            fi
            LUKS_PASS_CONFIRM=$(gum input --password --placeholder "confirm LUKS password...")
            if [ "$LUKS_PASS" != "$LUKS_PASS_CONFIRM" ]; then
                gum log --level error "Passwords do not match."
                exit 1
            fi
            echo "$LUKS_PASS" | pass insert -m "machine/${MACHINE}/luks/key/0"
            gum log --level info "LUKS password stored in pass"
        fi

        if ! pass show "machine/${MACHINE}/solomon/ssh/private-key" >/dev/null 2>&1; then
            gum log --level info "No user SSH keys found for ${MACHINE}, generating..."
            ssh_temp=$(mktemp -d)
            ssh-keygen -t ed25519 -C "solomon@${MACHINE}" -f "${ssh_temp}/id_ed25519" -N ""
            pass insert -m "machine/${MACHINE}/solomon/ssh/private-key" < "${ssh_temp}/id_ed25519"
            pass insert -m "machine/${MACHINE}/solomon/ssh/public-key" < "${ssh_temp}/id_ed25519.pub"
            rm -rf "$ssh_temp"
            gum log --level info "User SSH keys generated and stored in pass"
        fi
    fi

    # ── 5. SSH connectivity test ──────────────────────────────────────
    gum log --level info "Testing SSH connectivity to ${IP}..."
    if ! ssh -o ConnectTimeout=10 -o BatchMode=yes "root@${IP}" true; then
        gum log --level error "Cannot connect to root@${IP}"
        gum log --level error "Please ensure:"
        gum log --level error "  - The machine is booted from the NixOS ISO"
        gum log --level error "  - SSH is running and accessible"
        gum log --level error "  - Your SSH keys are authorized for root"
        exit 1
    fi
    gum log --level info "SSH connectivity confirmed"

    # ── 6. Generate hardware.nix ─────────────────────────────────────
    gum log --level info "Generating hardware configuration..."
    ssh "root@${IP}" nixos-generate-config --no-filesystems --show-hardware-config > "${TARGET}/hardware.nix"

    if [ ! -f "${TARGET}/hardware.nix" ]; then
        gum log --level error "CRITICAL: hardware.nix was not created"
        gum log --level error "SSH command may have failed"
        exit 1
    elif [ ! -s "${TARGET}/hardware.nix" ]; then
        gum log --level error "CRITICAL: hardware.nix is empty"
        gum log --level error "Hardware detection may have failed on the target machine"
        exit 1
    fi
    gum log --level info "Hardware configuration generated successfully"

    gum log --level info "Adding hardware configuration to git..."
    if ! git add "${TARGET}/hardware.nix"; then
        gum log --level error "CRITICAL: Failed to add hardware.nix to git"
        gum log --level error "Nix flakes require all files to be git-tracked"
        gum log --level error "Without this, the installation will fail"
        gum log --level error "Please ensure you're in a clean git repository and try again"
        exit 1
    fi
    gum log --level info "Hardware configuration added to git successfully"

    # ── 7. Scaffold default.nix ──────────────────────────────────────
    if [ ! -f "${TARGET}/default.nix" ]; then
        HOST_ID=$(head -c4 /dev/urandom | od -A none -t x4 | tr -d ' ')
        gum log --level info "Generating default.nix with hostId ${HOST_ID}..."
        sed -e "s/{{MACHINE_NAME}}/${MACHINE}/g" -e "s/{{HOST_ID}}/${HOST_ID}/g" \
            "${DEFAULT_TEMPLATE}" > "${TARGET}/default.nix"

        if ! git add "${TARGET}/default.nix"; then
            gum log --level error "CRITICAL: Failed to add default.nix to git"
            exit 1
        fi
        gum log --level info "Default configuration generated and added to git successfully"
    else
        gum log --level info "default.nix already exists, skipping generation"
    fi

    # ── 8. Scaffold disk-config.nix ──────────────────────────────────
    if [ ! -f "${TARGET}/disk-config.nix" ]; then
        gum log --level info "Generating disk-config.nix from template..."
        cp "${DISK_TEMPLATE}" "${TARGET}/disk-config.nix"

        if ! git add "${TARGET}/disk-config.nix"; then
            gum log --level error "CRITICAL: Failed to add disk-config.nix to git"
            exit 1
        fi

        # ZFS-based templates import zfs-pool.nix; copy it alongside disk-config.nix.
        if [[ "${DISK_TEMPLATE}" == *zfs* ]]; then
            cp "installer/templates/zfs-pool.nix" "${TARGET}/zfs-pool.nix"
            if ! git add "${TARGET}/zfs-pool.nix"; then
                gum log --level error "CRITICAL: Failed to add zfs-pool.nix to git"
                exit 1
            fi
        fi
        gum log --level info "Disk configuration generated and added to git successfully"
    else
        gum log --level info "disk-config.nix already exists, skipping generation"
    fi

    # ── 9. SSH host key generation / retrieval ───────────────────────
    temp=$(mktemp -d)

    # Not "install -d": the wrapper puts this package's own bin/install first in PATH.
    mkdir -p -m755 "$temp/etc/ssh"

    if pass show "machine/${MACHINE}/ssh-host-key/ed25519/private" >/dev/null 2>&1; then
        gum log --level info "SSH keys already exist for ${MACHINE}, using existing keys"
    else
        gum log --level info "Generating new SSH host keys for ${MACHINE}"
        ssh-keygen -t ed25519 -C "solomon@${MACHINE}" -f "${temp}/etc/ssh/ssh_host_ed25519_key" -N ""
        ssh-keygen -t rsa -C "solomon@${MACHINE}" -f "${temp}/etc/ssh/ssh_host_rsa_key" -N ""

        gum log --level info "Storing SSH host keys in pass"
        pass insert -m "machine/${MACHINE}/ssh-host-key/ed25519/private" < "${temp}/etc/ssh/ssh_host_ed25519_key"
        pass insert -m "machine/${MACHINE}/ssh-host-key/ed25519/public" < "${temp}/etc/ssh/ssh_host_ed25519_key.pub"
        pass insert -m "machine/${MACHINE}/ssh-host-key/rsa/private" < "${temp}/etc/ssh/ssh_host_rsa_key"
        pass insert -m "machine/${MACHINE}/ssh-host-key/rsa/public" < "${temp}/etc/ssh/ssh_host_rsa_key.pub"
    fi

    gum log --level info "Retrieving SSH keys from pass for deployment"
    pass "machine/${MACHINE}/ssh-host-key/ed25519/private" > "$temp/etc/ssh/ssh_host_ed25519_key"
    pass "machine/${MACHINE}/ssh-host-key/ed25519/public"  > "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    pass "machine/${MACHINE}/ssh-host-key/rsa/private"      > "$temp/etc/ssh/ssh_host_rsa_key"
    pass "machine/${MACHINE}/ssh-host-key/rsa/public"       > "$temp/etc/ssh/ssh_host_rsa_key.pub"

    chmod 600 "$temp/etc/ssh/ssh_host_ed25519_key"
    chmod 644 "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    chmod 600 "$temp/etc/ssh/ssh_host_rsa_key"
    chmod 644 "$temp/etc/ssh/ssh_host_rsa_key.pub"

    # ── 10. Run nixos-anywhere ──────────────────────────────────────
    gum log --level info "Installing NixOS to the host system with our secrets"
    if [ "$IS_PC" -eq 1 ]; then
        nixos-anywhere \
            --extra-files "$temp" \
            --disk-encryption-keys /tmp/disk.key <(pass "machine/${MACHINE}/luks/key/0") \
            --flake ".#${MACHINE}" "root@${IP}" \
            --no-reboot
    else
        nixos-anywhere \
            --extra-files "$temp" \
            --flake ".#${MACHINE}" "root@${IP}"
    fi

    # The VM now boots from its disk, so it does not need the ISO.
    # The installed system has the host keys from pass, not the ISO keys.
    if [ -n "$TRUENAS_VM_ID" ]; then
        detach_installer_iso "$TRUENAS_VM_ID"
        ssh-keygen -R "${IP}" >/dev/null 2>&1 || true
    fi

    # ── 11. PC post-install (before reboot) ─────────────────────────
    if [ "$IS_PC" -eq 1 ]; then
        # NixOS default UID/GID for the first declared user.
        LOCAL_USER="solomon"
        LOCAL_UID=1000
        LOCAL_GID=100

        gum log --level info "Mount ZFS datasets to /mnt"
        ssh "root@${IP}" <<EOF
          set -e
          mount.zfs tank/root /mnt
          mkdir -p /mnt/var/log
          mount.zfs tank/systemd-logs /mnt/var/log
          mount.zfs tank/nix /mnt/nix
          mount.zfs tank/home /mnt/home
          mkdir -p /mnt/home/${LOCAL_USER}/.ssh
EOF
        gum log --level info "Copy user SSH keys"
        pass "machine/${MACHINE}/solomon/ssh/private-key" | ssh "root@${IP}" "cat > /mnt/home/${LOCAL_USER}/.ssh/id_ed25519"
        pass "machine/${MACHINE}/solomon/ssh/public-key"  | ssh "root@${IP}" "cat > /mnt/home/${LOCAL_USER}/.ssh/id_ed25519.pub"
        ssh "root@${IP}" "chmod -R 700 /mnt/home/${LOCAL_USER}/.ssh && chown -R ${LOCAL_UID}:${LOCAL_GID} /mnt/home/${LOCAL_USER}/.ssh"

        gum log --level info "Copy flake repo"
        rsync -chavzP ./ "root@${IP}:/mnt/etc/nixos/flake/"
        ssh "root@${IP}" "chown -R ${LOCAL_UID}:${LOCAL_GID} /mnt/etc/nixos/flake"

        gum log --level info "Copy GPG"
        rsync -chavzP "/home/${LOCAL_USER}/.gnupg" "root@${IP}:/mnt/home/${LOCAL_USER}/"
    fi

    # ── 12. Register with SOPS ──────────────────────────────────────
    # .sops.yaml and secrets.yaml are git-tracked, so if anything goes
    # wrong here just `git checkout .sops.yaml secrets.yaml` to recover.
    SOPS_UPDATED=0

    # Register machine system key (all machine types)
    if [ "$MACHINE_DIR" = "personal-computers" ]; then
        SYS_ANCHOR="pc_${MACHINE}"
    else
        SYS_ANCHOR="server_${MACHINE}"
    fi

    if yq ".keys[] | anchor" .sops.yaml | grep -q "^${SYS_ANCHOR}$"; then
        gum log --level info "SOPS anchor &${SYS_ANCHOR} already exists, skipping"
    else
        SYS_AGE_KEY=$(pass "machine/${MACHINE}/ssh-host-key/ed25519/public" | ssh-to-age)
        if [[ ! "$SYS_AGE_KEY" =~ ^age1[a-z0-9]{58}$ ]]; then
            gum log --level error "Invalid age key derived for system: ${SYS_AGE_KEY}"
            exit 1
        fi
        gum log --level info "Derived system age key for ${MACHINE}: ${SYS_AGE_KEY}"
        yq -i ".keys += \"${SYS_AGE_KEY}\" | .keys[-1] anchor = \"${SYS_ANCHOR}\"" .sops.yaml
        yq -i ".creation_rules[0].key_groups[0].age += alias(\"${SYS_ANCHOR}\")" .sops.yaml
        SOPS_UPDATED=1
    fi

    # Register user key (PCs only)
    if [ "$IS_PC" -eq 1 ]; then
        USER_ANCHOR="solomon_${MACHINE}"

        if yq ".keys[] | anchor" .sops.yaml | grep -q "^${USER_ANCHOR}$"; then
            gum log --level info "SOPS anchor &${USER_ANCHOR} already exists, skipping"
        else
            USER_AGE_KEY=$(pass "machine/${MACHINE}/solomon/ssh/public-key" | ssh-to-age)
            if [[ ! "$USER_AGE_KEY" =~ ^age1[a-z0-9]{58}$ ]]; then
                gum log --level error "Invalid age key derived for user: ${USER_AGE_KEY}"
                exit 1
            fi
            gum log --level info "Derived user age key for ${MACHINE}: ${USER_AGE_KEY}"
            yq -i ".keys += \"${USER_AGE_KEY}\" | .keys[-1] anchor = \"${USER_ANCHOR}\"" .sops.yaml
            yq -i ".creation_rules[0].key_groups[0].age += alias(\"${USER_ANCHOR}\")" .sops.yaml
            SOPS_UPDATED=1
        fi
    fi

    if [ "$SOPS_UPDATED" -eq 1 ]; then
        sops updatekeys --yes secrets.yaml
        gum log --level info "SOPS keys registered and secrets updated"
    fi

    # ── 13. Reboot (PC only — servers already rebooted by nixos-anywhere) ─
    if [ "$IS_PC" -eq 1 ]; then
        ssh "root@${IP}" 'reboot now' || true
    fi

    if [ "$VNC_PASSWORD_MADE" -eq 1 ]; then
        gum log --level info "VNC password for the TrueNAS console is in pass at machine/${MACHINE}/vnc-password."
    fi
}

# Run main only when the script runs, not when a test sources it.
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    main
fi
