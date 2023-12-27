{ pkgs, ... }:

let
  plankaLocation = "/mnt/planka";
  userAvatars = "${plankaLocation}/user-avatars";
  projectBackgroundImages = "${plankaLocation}/project-background-images";
  attachaments = "${plankaLocation}/attachaments";
in
{
  fileSystems."/mnt/planka" = {
    device = "192.168.5.6:/mnt/tank/planka";
    fsType = "nfs";
  };

  virtualisation.oci-containers.containers = {
    planka = {
      image = "ghcr.io/plankanban/planka:latest";
      extraOptions = [ "--network=planka-bridge" ];
      ports = [ "8085:1337" ];

      volumes = [
        "${userAvatars}:/app/public/user-avatars"
        "${projectBackgroundImages}:/app/public/project-background-images"
        "${attachaments}:/app/private/attachements"
      ];

      environment = {
        BASE_URL = "http://planka.service";
        TRUST_PROXY = "1";
        # TODO: SOPS-Nix:
        DATABASE_URL = "postgresql://planka_admin:hunter2@transfigured-night/planka";
        SECRET_KEY = "hunter2";
      };

      autoStart = true;
    };
  };

  systemd.services.init-planka-network = {
    description = "Create the network bridge for planka.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = ''
      # Put a true at the end to prevent getting non-zero return code, which will
      # crash the whole service.
      check=$(${pkgs.docker}/bin/docker network ls | grep "planka-bridge" || true)
      if [ -z "$check" ];
        then ${pkgs.docker}/bin/docker network create planka-bridge
        else echo "planka-bridge already exists in docker"
      fi
    '';
  };

  services.nginx.virtualHosts."planka.service" = {
    locations."/" = {
      proxyPass = "http://localhost:8085";
      proxyWebsockets = true;
    };
  };
}
