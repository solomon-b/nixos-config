{ ... }:

{
  services = {
    lidarr = {
      enable = true;
      openFirewall = true;
    };

    prowlarr = {
      enable = true;
      openFirewall = true;
    };

    radarr = {
      enable = true;
      openFirewall = true;
    };
  };

  fileSystems."/mnt/media" = {
    device = "192.168.5.6:/mnt/tank/Media ";
    fsType = "nfs";
  };
}
