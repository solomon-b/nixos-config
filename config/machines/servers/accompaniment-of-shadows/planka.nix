{ config, pkgs, ... }:

let
  plankaLocation = "/mnt/planka";
  userAvatars = "${plankaLocation}/user-avatars";
  backgroundImages = "${plankaLocation}/background-images";
  attachments = "${plankaLocation}/attachments";
  favicons = "${plankaLocation}/favicons";
  data = "${plankaLocation}/data";
in
{
  fileSystems."/mnt/planka" = {
    device = "192.168.5.6:/mnt/tank/app-data/planka";
    fsType = "nfs";
    options = [
      "defaults" # → rw,suid,dev,exec,auto,nouser,async
      "vers=3" # force NFSv3
      "proto=tcp" # use TCP transport
      "intr" # allow signals (Ctrl-C) to interrupt
      "timeo=30" # initial timeout = 3 s (30 deciseconds)
      "retrans=3" # retry only 3 times (~9 s total)
      "_netdev" # wait for network before mounting
    ];
  };

  virtualisation.oci-containers.containers = {
    planka = {
      image = "ghcr.io/plankanban/planka:2.1.1";
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "ghcr.io/plankanban/planka";
        imageDigest = "sha256:19b507ae3ab5cb1855c3f6984249e4a4881ed0b912febdfd492139c29bf10f39";
        sha256 = "sha256-QonM7MmBmx/nvGymd4+Hh0pDa3a0E6UuDETRURqALg8=";
        finalImageTag = "2.1.1";
      };

      extraOptions = [
        "--network=host"
      ];

      volumes = [
        "${userAvatars}:/app/public/user-avatars"
        "${backgroundImages}:/app/public/background-images"
        "${attachments}:/app/private/attachments"
        "${favicons}:/app/public/favicons"
        "${data}:/app/data"
        "${config.sops.secrets.planka-secret-key.path}:/run/secrets/planka-secret-key:ro"
        "${config.sops.secrets.planka-database-password.path}:/run/secrets/planka-database-password:ro"
      ];

      environment = {
        BASE_URL = "http://planka.service.home.arpa";
        TRUST_PROXY = "true";
        DATABASE_URL = "postgresql://planka_admin:hunter2@localhost/planka";
        SECRET_KEY__FILE = "/run/secrets/planka-secret-key";
        DATABASE_PASSWORD__FILE = "/run/secrets/planka-database-password";
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

  # The Planka application is:
  #
  #   UID: 1000
  #   Username: node
  #
  # This means I can't create a matching user in the host system for assigning
  # SOPS secret permissions.
  sops.secrets.planka-secret-key = {
    mode = "0444";
  };
  sops.secrets.planka-database-password = {
    mode = "0444";
  };
}
