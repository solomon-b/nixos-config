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

    # Generate hardware config
    ssh "root@${IP}" nixos-generate-config --no-filesystems --show-hardware-config > "config/machines/servers/${MACHINE}/hardware.nix"
    git add "config/machines/servers/${MACHINE}/hardware.nix"

    # Create a temporary directory
    temp=$(mktemp -d)
    trap cleanup EXIT

    # Create the directory where sshd expects to find the host keys
    install -d -m755 "$temp/etc/ssh"

    # Generate new ssh host keys for the machine
    echo "Generating ssh keys for ${MACHINE}"
    ssh-keygen -t ed25519 -C "solomon@${MACHINE}" -f "${temp}/etc/ssh/ssh_host_ed25519_key"
    ssh-keygen -t rsa -C "solomon@${MACHINE}" -f "${temp}/etc/ssh/ssh_host_rsa_key"

    # Insert new keys into `pass`
    cat "${temp}/etc/ssh/ssh_host_ed25519_key" | pass insert --echo "machine/${MACHINE}/ssh-host-key/ed25519/private"
    cat "${temp}/etc/ssh/ssh_host_ed25519_key.pub" | pass insert --echo "machine/${MACHINE}/ssh-host-key/ed25519/public"

    cat "${temp}/etc/ssh/ssh_host_rsa_key" | pass insert --echo "machine/${MACHINE}/ssh-host-key/rsa/private"
    cat "${temp}/etc/ssh/ssh_host_rsa_key.pub" | pass insert --echo "machine/${MACHINE}/ssh-host-key/rsa/public"

    # Set the correct permissions so sshd will accept the keys
    chmod 600 "$temp/etc/ssh/ssh_host_ed25519_key"
    chmod 644 "$temp/etc/ssh/ssh_host_ed25519_key.pub"
    chmod 600 "$temp/etc/ssh/ssh_host_rsa_key"
    chmod 644 "$temp/etc/ssh/ssh_host_rsa_key.pub"

    # Install NixOS to the host system with our secrets
    nix run github:numtide/nixos-anywhere -- --extra-files "$temp" --flake ".#${MACHINE}" "root@${IP}"
}

main
