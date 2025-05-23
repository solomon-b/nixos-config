{ pkgs, ... }:

let
  dbHostname = "192.168.5.101"; # transfigured-night
  dbUsername = "immich";
  dbPassword = "immich";
  dbDatabaseName = "immich";

  redisHostname = "192.168.5.101"; # transfigured-night
  redisPassword = "hunter2";
  photosLocation = "/mnt/immich";

  immichWebUrl = "http://immich_web:3000";
  immichServerUrl = "http://immich_server:3001";
  immichMachineLearningUrl = "http://immich_machine_learning:3003";

  typesenseApiKey = "abcxyz123";

  environment = {
    DB_HOSTNAME = dbHostname;
    DB_USERNAME = dbUsername;
    DB_PASSWORD = dbPassword;
    DB_DATABASE_NAME = dbDatabaseName;

    REDIS_HOSTNAME = redisHostname;
    REDIS_PASSWORD = redisPassword;

    UPLOAD_LOCATION = photosLocation;

    IMMICH_WEB_URL = immichWebUrl;
    IMMICH_SERVER_URL = immichServerUrl;
    IMMICH_MACHINE_LEARNING_URL = immichMachineLearningUrl;

  };
in
{
  fileSystems."/mnt/immich" = {
    device = "192.168.5.6:/mnt/tank/app-data/immich";
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
    immich_server = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "ghcr.io/immich-app/immich-server";
        imageDigest = "sha256:e36c27ba1e3cd20bf6038d0b6b330e35a9a5b7ce9ee57db70a97bc49ca41ea6b";
        sha256 = "sha256-phvsGAlPoh0RQQfkT9w3x+XYhTLp/U7JofETkJONrrM=";
      };
      image = "ghcr.io/immich-app/immich-server";

      ports = [ "8084:2283" ];


      volumes = [
        "${photosLocation}:/usr/src/app/upload"
        "/etc/localtime:/etc/localtime:ro"
      ];

      environment = environment;

      extraOptions = [ "--network=immich-bridge" ];

      # config = {
      #   WorkingDir = "/usr/src/app";
      #   Entrypoint = [ "tini" "--" "/bin/bash" ];
      #   Cmd = [ "start.sh" ];
      # };

      autoStart = true;
    };

    immich_machine_learning = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "ghcr.io/immich-app/immich-machine-learning";
        imageDigest = "sha256:e00799ab188850df8d467723017610133c9e7e87fae454a3b015b8896093efa5";
        sha256 = "sha256-zObsW6cR/5OT3XndOG0uoJDT3cJlGhpYavYbNZIRv/U=";
      };
      image = "ghcr.io/immich-app/immich-machine-learning";
      extraOptions = [ "--network=immich-bridge" ];

      environment = environment;

      volumes = [
        "${photosLocation}:/usr/src/app/upload"
        "model-cache:/cache"
      ];

      autoStart = true;
    };
  };

  systemd.services.init-immich-network = {
    description = "Create the network bridge for immich.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = ''
      # Put a true at the end to prevent getting non-zero return code, which will
      # crash the whole service.
      check=$(${pkgs.docker}/bin/docker network ls | grep "immich-bridge" || true)
      if [ -z "$check" ];
        then ${pkgs.docker}/bin/docker network create immich-bridge
        else echo "immich-bridge already exists in docker"
      fi
    '';
  };


  services.nginx.virtualHosts."immich.service.home.arpa" = {
    locations."/" = {
      proxyPass = "http://localhost:8084";
      extraConfig = ''
        client_max_body_size 0;
        proxy_max_temp_file_size 96384m;
      '';
    };
  };
}
