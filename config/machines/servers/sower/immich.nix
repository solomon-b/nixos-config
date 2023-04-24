{ pkgs, ... }:

let

  immichServerUrl = "localhost";
  immichWebUrl =  "localhost";
  typesenseApiKey = "";

  dbHostname = "192.168.5.101"; # transfigured-night
  dbUsername = "immich";
  dbPassword = "immich";
  dbDatabaseName = "immich";

  redisHostname = "192.168.5.101"; # transfigured-night
  redisPassword = "hunter2";
  photosLocation = "/mnt/storage/Immich";
in
{
  virtualisation.oci-containers.containers.immich = {
    image = "ghcr.io/imagegenius/immich:latest";

    environment = {
      PUID = "1000";
      PGID = "1000";
      TZ = "America/Los_Angeles";
      DB_HOSTNAME = dbHostname;
      DB_USERNAME = dbUsername;
      DB_PASSWORD = dbPassword;
      DB_DATABASE_NAME = dbDatabaseName;
      REDIS_HOSTNAME = redisHostname;
      REDIS_PASSWORD = redisPassword;
      JWT_SECRET = "abcdefghijkzlmnopqrstuvwxyz";
    };

    volumes = [
      "immichdata:/config"
      "${photosLocation}:/photos" 
    ];

    ports = [ "8084:8080" ];
  };

  services.nginx.virtualHosts."immich.service" = {
    locations."/".proxyPass = "http://localhost:8084";
  };
}
