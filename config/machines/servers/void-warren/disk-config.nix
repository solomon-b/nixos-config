{
  # Construct the partition table for Raspberry Pi's primary disk (SD card or USB).
  # Note: For Raspberry Pi, we use a simpler layout without ZFS due to ARM constraints.
  disko.devices = {
    disk.main = {
      type = "disk";
      device = "/dev/mmcblk0"; # SD card device, change to /dev/sda for USB boot
      content = {
        type = "gpt";
        partitions = {
          # Raspberry Pi firmware partition
          firmware = {
            size = "512M";
            type = "EF00"; # EFI System Partition
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "fmask=0022" "dmask=0022" ];
            };
          };
          # Root filesystem
          root = {
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
            };
          };
        };
      };
    };
  };
}
