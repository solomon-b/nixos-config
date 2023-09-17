{ ... }:

{
  fileSystems."/mnt/media" = {
    device = "192.168.5.6:/mnt/tank/Media";
    fsType = "nfs";
  };

  fileSystems."/mnt/storage" = {
    device = "192.168.5.6:/mnt/tank/solomon";
    fsType = "nfs";
  };
}
