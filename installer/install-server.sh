#!/bin/bash
# https://github.com/wagdav/homelab/blob/master/installer/install.sh

set -ex

setup_partitions () {
    echo "Setting up partitions.."

    parted /dev/vda -- mklabel gpt
    parted /dev/vda -- mkpart primary 512MiB -2GiB
    parted /dev/vda -- mkpart primary linux-swap -2GiB 100%
    parted /dev/vda -- mkpart ESP fat32 1MiB 512MiB
    parted /dev/vda -- set 3 boot on

    mkfs.ext4 -L nixos /dev/vda1
    mkswap -L swap /dev/vda2
    swapon /dev/vda2
    mkfs.fat -F 32 -n boot /dev/vda3        # (for UEFI systems only)
    mount /dev/disk/by-label/nixos /mnt
    mkdir -p /mnt/boot                      # (for UEFI systems only)
    mount /dev/disk/by-label/boot /mnt/boot # (for UEFI systems only)

    echo "..done"
}

setup_config () {
    echo "Setting up initial nixos config.."
    
    nixos-generate-config --root /mnt

    cp /etc/configuration.nix /mnt/etc/nixos/configuration.nix

    mkdir /mnt/secrets

    cp /etc/primary-user-password /mnt/secrets/primary-user-password
    cp /etc/id_ed25519.pub /mnt/secrets/id_ed25519.pub
    cp /etc/id_ed25519 /mnt/secrets/id_ed25519

    echo "..done"
}


main () {
    setup_partitions
    setup_config
    nixos-install
    reboot
}

main
