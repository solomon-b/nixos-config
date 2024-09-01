{ pkgs, ... }:

let
  plankaLocation = "/mnt/planka";
  userAvatars = "${plankaLocation}/user-avatars";
  projectBackgroundImages = "${plankaLocation}/project-background-images";
  attachaments = "${plankaLocation}/attachaments";
in
{
  fileSystems."/mnt/planka" = {
    device = "192.168.5.6:/mnt/tank/app-data/planka";
    fsType = "nfs";
  };

  virtualisation.oci-containers.containers = {
    planka = {
      image = "ghcr.io/plankanban/planka:latest";
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

  services.nginx.virtualHosts."planka.service" = {
    locations."/" = {
      proxyPass = "http://localhost:8085";
      proxyWebsockets = true;
    };
  };
}
