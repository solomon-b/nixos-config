{ config, pkgs, ... }:

let
  dbHostname = "192.168.5.101"; # transfigured-night
  dbUsername = "immich";
  dbPassword = "immich";
  dbDatabaseName = "immich";

  redisHostname = "192.168.5.101"; # transfigured-night
  redisPassword = "hunter2";
  photosLocation = "/mnt/immich";

  typesenseApiKey = "abcxyz123";
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

  services.immich = {
    enable = true;
    machine-learning.enable = true;

    database = {
      enable = false;
    };

    redis = {
      # Note: see above WRT to `database.enable`.
      enable = false;
      host = redisHostname;
    };

    secretsFile = config.sops.secrets.immich-postgres-password.path;

    environment = {
      # PostgreSQL
      DB_HOSTNAME = dbHostname;
      DB_USERNAME = dbUsername;
      DB_PASSWORD = dbPassword;
      DB_DATABASE_NAME = dbDatabaseName;

      # Redis
      REDIS_HOSTNAME = redisHostname;
      REDIS_PASSWORD = redisPassword;

      # Upload path
      UPLOAD_LOCATION = photosLocation;
    };

    mediaLocation = photosLocation;
    settings.server.externalDomain = "http://immich.service.home.arpa";
    settings.newVersionCheck.enabled = true;
  };

  services.nginx.virtualHosts."immich.service.home.arpa" = {
    locations."/" = {
      proxyPass = "http://localhost:2283";
      extraConfig = ''
        client_max_body_size 0;
        proxy_max_temp_file_size 96384m;
      '';
    };
  };

  sops.secrets = {
    immich-postgres-password = { };
  };
}
