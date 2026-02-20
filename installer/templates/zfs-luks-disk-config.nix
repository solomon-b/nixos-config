{
  # Construct the partition table for the system's primary disk.
  disko.devices.disk.sda = {
    type = "disk";
    device = "/dev/sda";
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
        #
        # This system should be able to boot without manual intervention, so
        # the LUKS container will be set up to use a random segment data from
        # an external device constructed in a separate step.
        luks = {
          size = "100%";
          content = {
            type = "luks";
            name = "CRYPT";
            extraOpenArgs = [ ];
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
  disko.devices.zpool.tank = import ./zfs-pool.nix;
}
