{ pkgs, ... }:

let
  immichServer = {
    imageName = "ghcr.io/immich-app/immich-server";
    imageDigest = "sha256:ad7a9828eb25e4f42ad17631bc81408b3fe464c4eec2300742af2e37acb4e8d6";
    sha256 = "sha256-6hJkYK9Km8XmyWfl3MNydQVd8mnWNOta2WrLV+YUNho=";
  };

  immichMachineLearning = {
    imageName = "ghcr.io/immich-app/immich-machine-learning";
    imageDigest = "sha256:615e8208f48130aad83b9c47ac33627e0f080987885a3b9bc92dadaca8505735";
    sha256 = "sha256-0QaqS/VVSz6QqrUmMaHjo6NwuWbmSrwUz5QlxhEKJI0=";
  };

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

  wrapImage = { name, imageName, imageDigest, sha256, entrypoint }:
    pkgs.dockerTools.buildImage ({
      name = name;
      tag = "release";
      fromImage = pkgs.dockerTools.pullImage {
        imageName = imageName;
        imageDigest = imageDigest;
        sha256 = sha256;
      };
      created = "now";
      config =
        if builtins.length entrypoint == 0
        then null
        else {
          Cmd = entrypoint;
          WorkingDir = "/usr/src/app";
        };
    });
in
{
  fileSystems."/mnt/immich" = {
    device = "192.168.5.6:/mnt/tank/immich";
    fsType = "nfs";
  };

  virtualisation.oci-containers.containers = {
    immich_server = {
      imageFile = wrapImage {
        inherit (immichServer) imageName imageDigest sha256;

        name = "immich_server";
        entrypoint = [ "/bin/sh" "start-server.sh" ];
      };
      image = "immich_server:release";
      extraOptions = [ "--network=immich-bridge" ];

      volumes = [
        "${photosLocation}:/usr/src/app/upload"
      ];

      environment = environment;

      ports = [ "8084:3001" ];

      autoStart = true;
    };

    immich_microservices = {
      imageFile = wrapImage {
        inherit (immichServer) imageName imageDigest sha256;

        name = "immich_microservices";
        entrypoint = [ "/bin/sh" "start-microservices.sh" ];
      };
      image = "immich_microservices:release";
      extraOptions = [ "--network=immich-bridge" ];

      volumes = [
        "${photosLocation}:/usr/src/app/upload"
      ];

      environment = environment;

      autoStart = true;
    };

    immich_machine_learning = {
      imageFile = pkgs.dockerTools.pullImage immichMachineLearning;
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


  services.nginx.virtualHosts."immich.service" = {
    locations."/" = {
      proxyPass = "http://localhost:8084";
      extraConfig = ''
        client_max_body_size 0;
        proxy_max_temp_file_size 96384m;
      '';
    };
  };
}
