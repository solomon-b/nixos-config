#!/usr/bin/env bash

# https://nixos.wiki/wiki/Yubikey_based_Full_Disk_Encryption_(FDE)_on_NixOS#Partitioning
# https://github.com/sgillespie/nixos-yubikey-luks/tree/master
# https://gist.github.com/jkachmar/7305140b55ff1297703a2d2218964e54

set -ex

# Convert a raw binary string to a hexadecimal string
rbtohex() {
    ( od -An -vtx1 | tr -d ' \n' )
}

# Convert a hexadecimal string to a raw binary string
hextorb() {
    ( tr '[:lower:]' '[:upper:]' | sed -e 's/\([0-9A-F]\{2\}\)/\\\\\\x\1/gI'| xargs printf )
}

setup_yubikey () {
    echo "Setting up Yubikey.."

    # Configure the Yubikey
    SLOT=2
    export slot
    ykpersonalize -"$SLOT" -ochal-resp -ochal-hmac

    # Gather the initial salt for the PBA (set its length to what you find time-feasible on your machine).
    SALT_LENGTH=16
    salt="$(dd if=/dev/random bs=1 count=$SALT_LENGTH 2>/dev/null | rbtohex)"

    # Get the user passphrase used as the second factor in the PBA.
    # read -r -s k_user

    # Calculate the initial challenge and response to the YubiKey.
    challenge="$(echo -n "$salt" | openssl dgst -binary -sha512 | rbtohex)"
    response="$(ykchalresp -2 -x "$challenge" 2>/dev/null)"

    # Derive the Luks slot key from the two factors.
    KEY_LENGTH=512
    ITERATIONS=1000000

    # If you choose to authenticate with a user password, use the following line to generate the luks key.
    # k_luks="$(echo -n $k_user | pbkdf2-sha512 $(($KEY_LENGTH / 8)) $ITERATIONS $response | rbtohex)"

    # If you choose to authenticate without a user passphrase (not recommended), use this instead of the line above
    k_luks="$(echo | pbkdf2-sha512 $((KEY_LENGTH / 8)) $ITERATIONS "$response" | rbtohex)"

    echo "..done"
}

setup_partitions () {
    echo "Setting up partitions.."

    # Create a GPT partition table and two partitions on the target disk.
    # Partition 1: This will be the EFI system partition: 100MB-300MB
    # Partition 2: This will be the Luks-encrypted partition, aka the "luks device": Rest of your disk
    EFI_PART=/dev/sda1
    LUKS_PART=/dev/sda2

    # Create the necessary filesystem on the efi system partition, which will store the current salt for the PBA, and mount it.
    EFI_MNT=/root/boot
    mkdir "$EFI_MNT"
    mkfs.vfat -F 32 -n uefi "$EFI_PART"
    mount "$EFI_PART" "$EFI_MNT"

    # Decide where on the efi system partition to store the salt and prepare the directory layout accordingly.
    STORAGE=/crypt-storage/default
    mkdir -p "$(dirname $EFI_MNT$STORAGE)"

    # Store the salt and iteration count to the EFI systems partition.
    echo -ne "$salt\n$ITERATIONS" > $EFI_MNT$STORAGE

    # Create the LUKS device.
    CIPHER=aes-xts-plain64
    HASH=sha512
    echo -n "$k_luks" | hextorb | cryptsetup luksFormat --cipher="$CIPHER" \
    --key-size="$KEY_LENGTH" --hash="$HASH" --key-file=- "$LUKS_PART"

    echo "..done"
}

setup_lvm () {
    echo "Setting up LVM.."

    ## Setup the LUKS device as a physical volume.

    # The LUKS device first needs to be unlocked.
    echo -n "$k_luks" | hextorb | cryptsetup luksOpen $LUKS_PART crypt --key-file=-

    # Create the primary physical volume, as well as a system volume group, within the encrypted container.
    pvcreate /dev/mapper/crypt
    vgcreate system /dev/mapper/crypt

    # Create the logical volumes for 8 GB of swap space and the primary zpool.
    lvcreate -L 8G system -n swap
    lvcreate -l 100%FREE system -n pool

    # Create and enable swap space on the system-swap logical volume.
    mkswap /dev/mapper/system-swap
    swapon -d /dev/mapper/system-swap

    echo "..done"
}

setup_zpool () {
    echo "Creating ZFS ZPool.."

    # Create the zpool on top of the system-pool logical volume and verify that it's present with the expected settings.
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
          /dev/mapper/system-pool

    echo "..done"
}

setup_datasets () {
    echo "Creating ZFS datasets.."

    zfs create -p -v \
        -o relatime=off \
        -o mountpoint=legacy \
        -o com.sun:auto-snapshot=false \
        tank/nix

    zfs create -p -v \
        -o mountpoint=legacy \
        -o secondarycache=none \
	-o com.sun:auto-snapshot=true \
        tank/root

    zfs create -p -v \
        -o mountpoint=legacy \
        -o secondarycache=none \
	-o com.sun:auto-snapshot=true \
        tank/home

    zfs create -p -v \
        -o mountpoint=legacy \
        -o secondarycache=none \
        -o com.sun:auto-snapshot=false \
        tank/systemd-logs

    zfs create \
        -o refreservation=2G \
        -o primarycache=none \
        -o secondarycache=none \
        -o mountpoint=none \
        -o com.sun:auto-snapshot=false \
        tank/reserved

    echo "..done"
}

main () {
    #nix-shell https://github.com/sgillespie/nixos-yubikey-luks/archive/master.tar.gz
    setup_yubikey
    setup_partitions
    setup_lvm
    setup_zpool
    setup_datasets
}

main
