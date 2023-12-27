{ pkgs, ... }:

{
  services.rpcbind.enable = true;

  system.fsPackages = [ pkgs.nfs-utils ];

  systemd.mounts = [
    {
      type = "nfs";
      mountConfig = {
        Options = "noatime";
      };
      what = "192.168.5.6:/mnt/tank/Media";
      where = "/mnt/media";
    }
    {
      type = "nfs";
      mountConfig = {
        Options = "noatime";
      };
      what = "192.168.5.6:/mnt/tank/solomon";
      where = "/mnt/storage";
    }
  ];

  systemd.automounts = [
    {
      wantedBy = [ "multi-user.target" ];
      automountConfig = {
        TimeoutIdleSec = "600";
      };
      where = "/mnt/media";
    }
    {
      wantedBy = [ "multi-user.target" ];
      automountConfig = {
        TimeoutIdleSec = "600";
      };
      where = "/mnt/storage";
    }
  ];
}
