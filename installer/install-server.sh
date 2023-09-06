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
    MACHINE=$(ls config/machines/servers | gum choose)
    IP=$(gum input --placeholder "enter address..")

    # Create a temporary directory
    temp=$(mktemp -d)
    trap cleanup EXIT

    # Create the directory where sshd expects to find the host keys
    install -d -m755 "$temp/etc/ssh"

    # Decrypt your SSH host keys from the password store and copy it to the temporary directory
    pass "machine/${MACHINE}/ssh-host-key/ed25519/private" > "$temp/etc/ssh/ssh_host_ed25519_key"
    pass "machine/${MACHINE}/ssh-host-key/ed25519/public" > "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    pass "machine/${MACHINE}/ssh-host-key/rsa/private" > "$temp/etc/ssh/ssh_host_rsa_key"
    pass "machine/${MACHINE}/ssh-host-key/rsa/public" > "$temp/etc/ssh/ssh_host_rsa_key.pub"

    # Set the correct permissions so sshd will accept the keys
    chmod 600 "$temp/etc/ssh/ssh_host_ed25519_key"
    chmod 644 "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    chmod 600 "$temp/etc/ssh/ssh_host_rsa_key"
    chmod 644 "$temp/etc/ssh/ssh_host_rsa_key.pub"

    # Install NixOS to the host system with our secrets
    nix run github:numtide/nixos-anywhere -- --extra-files "$temp" --flake ".#${MACHINE}" "root@${IP}"
}

main
