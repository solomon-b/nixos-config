# Shared ZFS pool configuration for disk templates.
#
# Imported by zfs-disk-config.nix and zfs-luks-disk-config.nix as the value
# of `disko.devices.zpool.tank`.
{
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
}
