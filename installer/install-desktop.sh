#!/bin/sh
# https://github.com/wagdav/homelab/blob/master/installer/install.sh
# https://gist.github.com/jkachmar/7305140b55ff1297703a2d2218964e54

set -ex

parted /dev/nvme0n1 -- boot


# Create GPT partition table
parted /dev/nvme0n1 -- mklabel gpt
# Create primary partition
parted /dev/nvme0n1 -- mkpart primary 2GiB -8GiB
# Create 8GiB swap as p1
parted /dev/nvme0n1 -- mkpart primary linux-swap -8GiB 100%
# Create 2GiB Boot partion
parted /dev/nvme0n1 -- mkpart ESP fat32 1MiB 2GiB
parted /dev/nvme0n1 -- set 3 boot on

# Setup boot filesystem
mkfs.fat -F 32 -n BOOT /dev/nvme0n1p3

# Setup swap
mkswap -L swap /dev/nvme0n1p2
swapon /dev/nvme0n1p2

# Create the zfs pool
zpool create -f \
  -o ashift=12 \
  -o listsnapshots=on \
  -O acltype=posixacl \
  -O atime=off \
  -O compression=on \
  -O dnodesize=auto \
  -O normalization=formD \
  -O mountpoint=none \
  -O relatime=on \
  -O xattr=sa \
  tank \
  /dev/nvme0n1p1

# Create a ZFS dataset for the Nix store, and mount it.
zfs create -p -v \
  -o relatime=off \
  -o mountpoint=legacy \
  -o com.sun:auto-snapshot=false \
  tank/nix

# Create a ZFS dataset for the stateful user/system data that's meant
# to persist across reboots.
zfs create -p -v \
  -o mountpoint=legacy \
  -o secondarycache=none \
  -o com.sun:auto-snapshot=true \
  tank/home

zfs create -p -v \
  -o mountpoint=legacy \
  -o secondarycache=none \
  -o com.sun:auto-snapshot=true \
  tank/root

# It's not very useful to snapshot systemd logs, so that can have its
# own persistent dataset as well.
zfs create -p -v \
  -o mountpoint=legacy \
  -o secondarycache=none \
  -o com.sun:auto-snapshot=false \
  tank/systemd-logs

# Finally, create an unused, unmounted 2 GB dataset in case the rest
# of the pool runs out of space and is unable to reclaim it (an
# unfortunate side effect of copy-on-write filesystems).
#
# If that happens, this dataset can be deleted, space can be
# reclaimed, and then it can be created again in case something
# similar happens in the future.
zfs create \
  -o refreservation=2G \
  -o primarycache=none \
  -o secondarycache=none \
  -o mountpoint=none \
  tank/reserved

# Mount all the datasets onto /mnt
mount -t zfs tank/root /mnt

mkdir -p /mnt/boot                     
mount -t vfat /dev/disk/by-label/BOOT /mnt/boot

mkdir -p /mnt/home
mkdir -p /mnt/nix

mount -t zfs tank/nix /mnt/nix
mount -t zfs tank/home /mnt/home

mkdir -p /mnt/var/log
mount -t zfs tank/systemd-logs /mnt/var/log

nixos-generate-config --root /mnt

cp /etc/configuration.nix /mnt/etc/nixos/configuration.nix

mkdir /mnt/secrets

cp /etc/primary-user-password /mnt/secrets/primary-user-password
cp /etc/id_ed25519.pub /mnt/secrets/id_ed25519.pub
cp /etc/id_ed25519 /mnt/secrets/id_ed25519

nixos-install

reboot
