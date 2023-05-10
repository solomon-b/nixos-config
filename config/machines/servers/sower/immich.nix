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
        imageDigest = "sha256:0f611b58423ae6f544da506ff55683b0f947856a0f0bd85db0e8e48aa2837347";
        sha256 = "sha256-2iaSI42Ar5YDX/tolsWrOmeygCK/X74DZbWxaHhspow=";
        entrypoint = ["/bin/sh" "start-server.sh"];
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
        imageDigest = "sha256:0f611b58423ae6f544da506ff55683b0f947856a0f0bd85db0e8e48aa2837347";
        sha256 = "sha256-2iaSI42Ar5YDX/tolsWrOmeygCK/X74DZbWxaHhspow=";
        entrypoint = ["/bin/sh" "start-microservices.sh"];
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
        imageName =  "ghcr.io/immich-app/immich-machine-learning";
        imageDigest =  "sha256:0b16898ac1572d154673fb242e0727d0d1c82cba5948c0f1882c0ee7f56f94f2"; 
        sha256 = "sha256-eEUKyJjIywgf9LjTJ3Q2V79d+8cWIo3z59ZivNJ1sHA=";
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
        imageName =  "ghcr.io/immich-app/immich-web";
        imageDigest =  "sha256:f7d58ca0d8725ede4ceb67a640336b8189381878377d150065acffc94dbfb5c9"; 
        sha256 = "sha256-Ib0+GIa2aWa2r7gFmwj0Q7KSUkkVGJxWISdiTUJ2/pU=";
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
        imageName =  "ghcr.io/immich-app/immich-proxy";
        imageDigest =  "sha256:5e1b1b35cb8f63a9af896790871387703b1c0c8d9b308ab1290b8e615f0f84d2"; 
        sha256 =  "sha256-mgrVnoehuLJZQnonGKLcqL30oe8POGvnlaQ1r2PcuNQ=";
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
