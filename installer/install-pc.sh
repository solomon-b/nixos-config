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
    echo "Pick a machine:"
    MACHINE=$(ls config/machines/personal-computers | gum choose)
    IP=$(gum input --placeholder "enter address..")

    gum log --level info "Create a temporary directory"
    temp=$(mktemp -d)
    trap cleanup EXIT

    gum log --level info "Create the directory where sshd expects to find the host keys"
    install -d -m755 "$temp/etc/ssh"

    gum log --level info "Decrypt your SSH host keys from the password store and copy it to the temporary directory"
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
