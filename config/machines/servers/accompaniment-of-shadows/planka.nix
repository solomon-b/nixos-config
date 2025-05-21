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
    options = [
      "defaults"                  # → rw,suid,dev,exec,auto,nouser,async
      "vers=3"                    # force NFSv3
      "proto=tcp"                 # use TCP transport
      "intr"                      # allow signals (Ctrl-C) to interrupt
      "timeo=30"                  # initial timeout = 3 s (30 deciseconds)
      "retrans=3"                 # retry only 3 times (~9 s total)
      "_netdev"                   # wait for network before mounting
    ];
  };

  virtualisation.oci-containers.containers = {
    planka = {
      image = "ghcr.io/plankanban/planka:latest";

      extraOptions = [
        "--network=host"
      ];

      volumes = [
        "${userAvatars}:/app/public/user-avatars"
        "${projectBackgroundImages}:/app/public/project-background-images"
        "${attachaments}:/app/private/attachements"
      ];

      environment = {
        BASE_URL = "http://planka.service.home.arpa";
        TRUST_PROXY = "1";
        # TODO: SOPS-Nix:
        DATABASE_URL = "postgresql://planka_admin:hunter2@transfigured-night/planka";
        SECRET_KEY = "hunter2";
      };

      autoStart = true;
    };
  };

  services.nginx.virtualHosts."planka.service.home.arpa" = {
    locations."/" = {
      proxyPass = "http://localhost:1337";
      proxyWebsockets = true;
    };
  };
}
