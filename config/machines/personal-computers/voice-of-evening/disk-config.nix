{
  # Construct the partition table for the system's primary disk.
  disko.devices.disk.nvme0n1 = {
    type = "disk";
    device = "/dev/nvme0n1";
    content = {
      type = "gpt";
      partitions = {
        # Create a large boot partition.
        #
        # NixOS creates a separate boot entry for each generation, which
        # can fill up the partition faster than other operating systems.
        #
        # Storage is cheap, so this can be more generous than necessary.
        ESP = {
          size = "2G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "defaults" ];
          };
        };
        # Partition the remainder of the disk as a LUKS container.
        luks = {
          size = "100%";
          content = {
            type = "luks";
            name = "CRYPT";
            extraOpenArgs = [ ];
            passwordFile = "/tmp/disk.key";
            settings = {
              allowDiscards = true;
            };
            content = {
              type = "zfs";
              pool = "tank";
            };
          };
        };
      };
    };
  };

  # Construct the primary ZFS pool for this system.
  disko.devices.zpool.tank = {
    type = "zpool";

    options = {
      ashift = "12";
      autotrim = "on";
      listsnapshots = "on";
    };

    rootFsOptions = {
      acltype = "posixacl";
      atime = "off";
      canmount = "off";
      compression = "zstd";
      dnodesize = "auto";
      mountpoint = "none";
      normalization = "formD";
      relatime = "on";
      xattr = "sa";
      "com.sun:auto-snapshot" = "true";
    };

    datasets = {
      # Static reservation so the pool will never be 100% full.
      #
      # If a pool fills up completely, delete this & reclaim space; don't
      # forget to re-create it afterwards!
      reservation = {
        type = "zfs_fs";
        options = {
          canmount = "off";
          mountpoint = "none";
          refreservation = "2G";
          primarycache = "none";
          secondarycache = "none";
        };
      };

      # Root filesystem.
      root = {
        type = "zfs_fs";
        mountpoint = "/";
        options = {
          mountpoint = "legacy";
          secondarycache = "none";
          "com.sun:auto-snapshot" = "true";
        };
      };

      # `/nix/store` dataset; no snapshots required.
      nix = {
        type = "zfs_fs";
        mountpoint = "/nix";
        options = {
          mountpoint = "legacy";
          relatime = "off";
          secondarycache = "none";
          "com.sun:auto-snapshot" = "false";
        };
      };

      # User filesystem.
      home = {
        type = "zfs_fs";
        mountpoint = "/home";
        options = {
          mountpoint = "legacy";
          secondarycache = "none";
          "com.sun:auto-snapshot" = "true";
        };
      };

      # `journald` log.
      systemd-logs = {
        type = "zfs_fs";
        mountpoint = "/var/log";
        options = {
          mountpoint = "legacy";
          secondarycache = "none";
          "com.sun:auto-snapshot" = "false";
        };
      };
    };
  };
}
