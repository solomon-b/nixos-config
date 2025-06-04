#!/usr/bin/env bash

# https://github.com/nix-community/nixos-anywhere/blob/main/docs/howtos/secrets.md
# https://gist.github.com/jkachmar/7305140b55ff1297703a2d2218964e54
# https://github.com/jkachmar/termina/blob/main/hosts/tatl/disks.nix
# https://nixos.wiki/wiki/Yubikey_based_Full_Disk_Encryption_(FDE)_on_NixOS#Partitioning
# https://www.guyrutenberg.com/2022/02/17/unlock-luks-volume-with-a-yubikey/

set -e

# Function to cleanup temporary directory on exit
cleanup() {
  rm -rf "$temp"
}

main () {
    gum log --level info "Pick a machine:"
    MACHINE=$(ls config/machines/personal-computers | gum choose)
    IP=$(gum input --placeholder "enter address..")

    # Test SSH connectivity
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

    # Generate hardware config
    # NOTE: This was added for consistency with install-server.sh. Previously, the PC installer
    # expected hardware.nix to already exist. If this breaks existing workflows, this change
    # may need to be reverted (Chesterton's fence - there might be a reason this wasn't here).
    gum log --level info "Generating hardware configuration..."
    ssh "root@${IP}" nixos-generate-config --no-filesystems --show-hardware-config > "config/machines/personal-computers/${MACHINE}/hardware.nix"

    # Validate hardware config was generated successfully
    if [ ! -f "config/machines/personal-computers/${MACHINE}/hardware.nix" ]; then
        gum log --level error "CRITICAL: hardware.nix was not created"
        gum log --level error "SSH command may have failed"
        exit 1
    elif [ ! -s "config/machines/personal-computers/${MACHINE}/hardware.nix" ]; then
        gum log --level error "CRITICAL: hardware.nix is empty"
        gum log --level error "Hardware detection may have failed on the target machine"
        exit 1
    fi
    gum log --level info "Hardware configuration generated successfully"

    # Add hardware config to git (required for Nix flakes)
    gum log --level info "Adding hardware configuration to git..."
    if ! git add "config/machines/personal-computers/${MACHINE}/hardware.nix"; then
        gum log --level error "CRITICAL: Failed to add hardware.nix to git"
        gum log --level error "Nix flakes require all files to be git-tracked"
        gum log --level error "Without this, the installation will fail"
        gum log --level error "Please ensure you're in a clean git repository and try again"
        exit 1
    fi
    gum log --level info "Hardware configuration added to git successfully"

    # Generate default.nix if it doesn't exist
    if [ ! -f "config/machines/personal-computers/${MACHINE}/default.nix" ]; then
        gum log --level info "Generating default.nix configuration from template..."
        sed "s/{{MACHINE_NAME}}/${MACHINE}/g" installer/templates/pc-default.nix > "config/machines/personal-computers/${MACHINE}/default.nix"

        # Add default.nix to git
        if ! git add "config/machines/personal-computers/${MACHINE}/default.nix"; then
            gum log --level error "CRITICAL: Failed to add default.nix to git"
            exit 1
        fi
        gum log --level info "Default configuration generated and added to git successfully"
    else
        gum log --level info "default.nix already exists, skipping generation"
    fi

    # Generate disk-config.nix if it doesn't exist
    if [ ! -f "config/machines/personal-computers/${MACHINE}/disk-config.nix" ]; then
        gum log --level info "Generating disk-config.nix configuration from template..."
        cp installer/templates/pc-disk-config.nix "config/machines/personal-computers/${MACHINE}/disk-config.nix"

        # Add disk-config.nix to git
        if ! git add "config/machines/personal-computers/${MACHINE}/disk-config.nix"; then
            gum log --level error "CRITICAL: Failed to add disk-config.nix to git"
            exit 1
        fi
        gum log --level info "Disk configuration generated and added to git successfully"
    else
        gum log --level info "disk-config.nix already exists, skipping generation"
    fi

    gum log --level info "Create a temporary directory"
    temp=$(mktemp -d)
    trap cleanup EXIT

    gum log --level info "Create the directory where sshd expects to find the host keys"
    install -d -m755 "$temp/etc/ssh"

    # Check if SSH keys already exist in pass
    if pass show "machine/${MACHINE}/ssh-host-key/ed25519/private" >/dev/null 2>&1; then
        gum log --level info "SSH keys already exist for ${MACHINE}, using existing keys"
    else
        gum log --level info "Generate new ssh host keys for ${MACHINE}"
        ssh-keygen -t ed25519 -C "solomon@${MACHINE}" -f "${temp}/etc/ssh/ssh_host_ed25519_key" -N ""
        ssh-keygen -t rsa -C "solomon@${MACHINE}" -f "${temp}/etc/ssh/ssh_host_rsa_key" -N ""

        gum log --level info "Store SSH host keys in pass"
        cat "${temp}/etc/ssh/ssh_host_ed25519_key" | pass insert --echo "machine/${MACHINE}/ssh-host-key/ed25519/private"
        cat "${temp}/etc/ssh/ssh_host_ed25519_key.pub" | pass insert --echo "machine/${MACHINE}/ssh-host-key/ed25519/public"
        cat "${temp}/etc/ssh/ssh_host_rsa_key" | pass insert --echo "machine/${MACHINE}/ssh-host-key/rsa/private"
        cat "${temp}/etc/ssh/ssh_host_rsa_key.pub" | pass insert --echo "machine/${MACHINE}/ssh-host-key/rsa/public"
    fi

    # Retrieve keys from pass for deployment (whether new or existing)
    gum log --level info "Retrieving SSH keys from pass for deployment"
    pass "machine/${MACHINE}/ssh-host-key/ed25519/private" > "$temp/etc/ssh/ssh_host_ed25519_key"
    pass "machine/${MACHINE}/ssh-host-key/ed25519/public" > "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    pass "machine/${MACHINE}/ssh-host-key/rsa/private" > "$temp/etc/ssh/ssh_host_rsa_key"
    pass "machine/${MACHINE}/ssh-host-key/rsa/public" > "$temp/etc/ssh/ssh_host_rsa_key.pub"

    gum log --level info "Set the correct permissions so sshd will accept the keys"
    chmod 600 "$temp/etc/ssh/ssh_host_ed25519_key"
    chmod 644 "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    chmod 600 "$temp/etc/ssh/ssh_host_rsa_key"
    chmod 644 "$temp/etc/ssh/ssh_host_rsa_key.pub"

    gum log --level info "Install NixOS to the host system with our secrets"
    nix run github:numtide/nixos-anywhere -- \
	--extra-files "$temp" \
        --disk-encryption-keys /tmp/disk.key <(pass "machine/${MACHINE}/luks/key/0") \
	--flake ".#${MACHINE}" "root@${IP}" \
	--no-reboot

    gum log --level info "Mount ZFS datasets to /mnt"
    ssh "root@${IP}" <<EOF
      mount.zfs tank/root /mnt
      mkdir -p /mnt/var/log
      mount.zfs tank/systemd-logs /mnt/var/log
      mount.zfs tank/nix /mnt/nix
      mount.zfs tank/home /mnt/home
      mkdir -p /mnt/home/solomon/.ssh
EOF
    gum log --level info "Copy User SSH keys"
    pass "machine/${MACHINE}/solomon/ssh/private-key" | ssh "root@${IP}" 'cat > /mnt/home/solomon/.ssh/id_ed25519'
    pass "machine/${MACHINE}/solomon/ssh/public-key" | ssh "root@${IP}" 'cat > /mnt/home/solomon/.ssh/id_ed25519.pub'
    ssh "root@${IP}" <<EOF
      chmod -R 700 /mnt/home/solomon/.ssh
      chown -R 1000:100 /mnt/home/solomon/.ssh
EOF

    gum log --level info "Copy flake repo"
    rsync -chavzP  /etc/nixos/flake "root@${IP}:/mnt/etc/nixos/"
    ssh "root@${IP}" 'chown -R 1000:100 /mnt/etc/nixos/flake'

    gum log --level info "Copy GPG"
    rsync -chavzP /home/solomon/.gnupg "root@${IP}:/mnt/home/solomon/"

    ssh "root@${IP}" 'reboot now'
}

main
