{ pkgs, ... }:

let
  homeboxData = "/mnt/homebox";
in
{
  fileSystems."/mnt/homebox" = {
    device = "192.168.5.6:/mnt/tank/app-data/homebox";
    fsType = "nfs";
  };

  virtualisation.oci-containers.containers = {
    homebox = {
      image = "ghcr.io/hay-kot/homebox:latest";
      ports = [ "3100:7745" ];

      volumes = [
        "${homeboxData}:/data/"
      ];

      environment = {
        HBOX_LOG_LEVEL = "info";
        HBOX_LOG_FORMAT = "text";
        HBOX_WEB_MAX_UPLOAD_SIZE = "10";
      };

      autoStart = true;
    };
  };

  services.nginx.virtualHosts."homebox.service" = {
    locations."/" = {
      proxyPass = "http://localhost:3100";
      proxyWebsockets = true;
    };
  };
}
