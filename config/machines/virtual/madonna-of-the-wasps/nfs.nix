{ ... }:

{
  fileSystems."/mnt/Nextcloud_Data" = {
    device = "192.168.1.174:/mnt/tank/Nextcloud_Data";
    fsType = "nfs";
  };
}
