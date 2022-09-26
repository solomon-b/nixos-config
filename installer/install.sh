#!/bin/sh
# https://github.com/wagdav/homelab/blob/master/installer/install.sh

set -ex

parted /dev/sda -- mklabel gpt
parted /dev/sda -- mkpart primary 512MiB -8GiB
parted /dev/sda -- mkpart primary linux-swap -8GiB 100%
parted /dev/sda -- mkpart ESP fat32 1MiB 512MiB
parted /dev/sda -- set 3 boot on

mkfs.ext4 -L nixos /dev/sda1
mkswap -L swap /dev/sda2
swapon /dev/sda2
mkfs.fat -F 32 -n boot /dev/sda3        # (for UEFI systems only)
mount /dev/disk/by-label/nixos /mnt
mkdir -p /mnt/boot                      # (for UEFI systems only)
mount /dev/disk/by-label/boot /mnt/boot # (for UEFI systems only)

nixos-generate-config --root /mnt

cp /etc/configuration.nix /mnt/etc/nixos/configuration.nix

mkdir /mnt/secrets

cp /etc/primary-user-password /mnt/secrets/primary-user-password
cp /etc/id_ed25519.pub /mnt/secrets/id_ed25519.pub
cp /etc/id_ed25519 /mnt/secrets/id_ed25519

nixos-install

reboot
