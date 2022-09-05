{ ... }:

{
  fileSystems."/mnt/media" = {
    device = "192.168.1.174:/mnt/tank/Media ";
    fsType = "nfs";
  };

  fileSystems."/mnt/storage" = {
    device = "192.168.1.174:/mnt/tank/solomon";
    fsType = "nfs";
  };
}
