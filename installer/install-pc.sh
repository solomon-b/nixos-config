#!/usr/bin/env bash

# https://github.com/numtide/nixos-anywhere/blob/main/docs/howtos.md#secrets-and-full-disk-encryption
# https://gist.github.com/jkachmar/7305140b55ff1297703a2d2218964e54
# https://github.com/jkachmar/termina/blob/main/hosts/tatl/disks.nix
# https://nixos.wiki/wiki/Yubikey_based_Full_Disk_Encryption_(FDE)_on_NixOS#Partitioning

set -e

# Function to cleanup temporary directory on exit
cleanup() {
  rm -rf "$temp"
}

main () {
    echo "Pick a machine:"
    MACHINE=$(nix flake show --quiet --no-warn-dirty --json | jq -r '.nixosConfigurations | keys | join("\n")' | gum choose)
    IP=$(gum input --placeholder "enter address..")

    # Create a temporary directory
    temp=$(mktemp -d)
    trap cleanup EXIT

    # Create the directory where sshd expects to find the host keys
    install -d -m755 "$temp/etc/ssh"

    # Decrypt your private key from the password store and copy it to the temporary directory
    pass "machine/${MACHINE}/ssh-host-key/ed25519/private" > "$temp/etc/ssh/ssh_host_ed25519_key"
    pass "machine/${MACHINE}/ssh-host-key/ed25519/public" > "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    pass "machine/${MACHINE}/ssh-host-key/rsa/private" > "$temp/etc/ssh/ssh_host_rsa_key"
    pass "machine/${MACHINE}/ssh-host-key/rsa/public" > "$temp/etc/ssh/ssh_host_rsa_key.pub"

    # Set the correct permissions so sshd will accept the key
    chmod 600 "$temp/etc/ssh/ssh_host_ed25519_key"

    # Install NixOS to the host system with our secrets
    nix run github:numtide/nixos-anywhere -- --extra-files "$temp" --flake ".#${MACHINE}" "root@${IP}" --no-reboot

    # Copy User SSH keys
    ssh "root@${IP}" <<EOF
      mount.zfs tank/home /mnt/home
      mkdir -p /mnt/home/solomon/.ssh
EOF
    pass "machine/${MACHINE}/solomon/ssh/private-key" | ssh "root@${IP}" 'cat > /mnt/home/solomon/.ssh/id_ed25519'
    pass "machine/${MACHINE}/solomon/ssh/public-key" | ssh "root@${IP}" 'cat > /mnt/home/solomon/.ssh/id_ed25519.pub'
    ssh "root@${IP}" <<EOF
      chmod -R 700 /mnt/home/solomon/.ssh
      chown -R 1000:100 /mnt/home/solomon/.ssh
EOF

    # Copy flake repo
    rsync -a -essh /etc/nixos/flake "root@${IP}:/mnt/etc/nixos/"
    ssh "root@${IP}" 'chown -R 1000:100 /mnt/etc/nixos/flake'

    # Copy GPG
    rsync -a -essh /home/solomon/.gnupg "root@${IP}:/mnt/home/solomon/"

    #ssh "root@${IP}" 'reboot now'
}

main
