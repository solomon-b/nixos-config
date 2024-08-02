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
      extraOptions = [ "--network=homebox-bridge" ];
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

  systemd.services.init-homebox-network = {
    description = "Create the network bridge for homebox.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = ''
      # Put a true at the end to prevent getting non-zero return code, which will
      # crash the whole service.
      check=$(${pkgs.docker}/bin/docker network ls | grep "homebox-bridge" || true)
      if [ -z "$check" ];
        then ${pkgs.docker}/bin/docker network create homebox-bridge
        else echo "homebox-bridge already exists in docker"
      fi
    '';
  };

  services.nginx.virtualHosts."homebox.service" = {
    locations."/" = {
      proxyPass = "http://localhost:3100";
      proxyWebsockets = true;
    };
  };
}
