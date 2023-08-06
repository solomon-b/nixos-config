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

    TYPESENSE_API_KEY = typesenseApiKey;

    IMMICH_WEB_URL = immichWebUrl;
    IMMICH_SERVER_URL = immichServerUrl;
    IMMICH_MACHINE_LEARNING_URL = immichMachineLearningUrl;

  };

  wrapImage = { name, imageName, imageDigest, sha256, entrypoint}:
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
        name = "immich_server";
        imageName = "ghcr.io/immich-app/immich-server";
        imageDigest = "sha256:4fb6dfdef9a7595c1b8219fab9eb95caadf8f74dfd18094bb638aed09a7637c4";
        sha256 = "sha256-ItqBFSf81Tp93UwNfYp+EcNECCDO4K15cBxeudlkfqg=";
        entrypoint = [ "/bin/sh" "start-server.sh" ];
      };
      image = "immich_server:release";
      extraOptions = [ "--network=immich-bridge" ];
  
      volumes = [
        "${photosLocation}:/usr/src/app/upload" 
      ];
  
      environment = environment;
  
      dependsOn = [
        "typesense"
      ];
  
      autoStart = true;
    };
  
    immich_microservices = {
      imageFile = wrapImage {
        name = "immich_microservices";
        imageName = "ghcr.io/immich-app/immich-server";
        imageDigest = "sha256:4fb6dfdef9a7595c1b8219fab9eb95caadf8f74dfd18094bb638aed09a7637c4";
        sha256 = "sha256-ItqBFSf81Tp93UwNfYp+EcNECCDO4K15cBxeudlkfqg=";
        entrypoint = [ "/bin/sh" "start-microservices.sh" ];
      };
      image = "immich_microservices:release";
      extraOptions = [ "--network=immich-bridge" ];
  
      volumes = [
        "${photosLocation}:/usr/src/app/upload" 
      ];
  
      environment = environment;
  
      dependsOn = [
        "typesense"
      ];
  
      autoStart = true;
    };

    immich_machine_learning = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "ghcr.io/immich-app/immich-machine-learning";
        imageDigest = "sha256:9106bc706227bd6d1f7d20a68ebda1fcf5573097051ff908402976b998d52c5c";
        sha256 = "sha256-iwxMRvzCYi3yAEXqa8Duj05o7GoPSJZ6oeZ78Fq9xpQ=";
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
  
    immich_web = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "ghcr.io/immich-app/immich-web";
        imageDigest = "sha256:e13fb6c45f0e15b7fffed28772b5cbb627fdd07bbef4e7029acac1b3bdd29168";
        sha256 = "sha256-GL02/jI2P5pNsBEsWmIRrEhH6Hartc6sZTw9wQe1InE=";
      };
      image = "ghcr.io/immich-app/immich-web";
      extraOptions = [ "--network=immich-bridge" ];
  
      environment = environment;
  
      autoStart = true;
    };
  
  
    typesense = {
      image = "typesense/typesense:0.24.0";
      extraOptions = [ "--network=immich-bridge" ];
  
      environment = {
        TYPESENSE_API_KEY = typesenseApiKey;
        TYPESENSE_DATA_DIR = "/data";
      };
      
      log-driver = "none";
  
      volumes = [
        "tsdata:/data"
      ];
  
      autoStart = true;
    };
  
    immich_proxy = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "ghcr.io/immich-app/immich-proxy";
        imageDigest = "sha256:ada30fb2d4fd5840f0d28b0a5ae211658b639478cd089b0bb98df1e865c82d27";
        sha256 = "sha256-SQUVCT6CNynNQ1eIAXSHToBvjfB8NvnYFp19Bfyfo8c=";
      };
      image = "ghcr.io/immich-app/immich-proxy:release";
      extraOptions = [ "--network=immich-bridge" ];
  
      environment = {
        IMMICH_SERVER_URL = immichServerUrl;
        IMMICH_WEB_URL = immichWebUrl;
        IMMICH_MACHINE_LEARNING_URL = immichMachineLearningUrl;
      };
    
      log-driver = "none";
  
      dependsOn = [
        "typesense"
      ];
  
      ports = [ "8084:8080" ];
  
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
