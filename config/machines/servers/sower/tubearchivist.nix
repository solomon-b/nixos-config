{ config, pkgs, ... }:

let
  media = "/mnt/tubearchivist/media";
  cache = "/mnt/tubearchivist/cache";
in
{
  fileSystems."/mnt/tubearchivist/media" = {
    device = "192.168.5.6:/mnt/tank/tubearchivist/media";
    fsType = "nfs";
  };

  fileSystems."/mnt/tubearchivist/cache" = {
    device = "192.168.5.6:/mnt/tank/tubearchivist/cache";
    fsType = "nfs";
  };

  virtualisation.oci-containers.containers = {
    tubearchivist = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName =  "bbilly1/tubearchivist";
        imageDigest =  "sha256:0653c43ef7ca887fd73a7d1b7b9fae4a6e0cfa32fdeac965179aee47c1c4ee83"; 
        sha256 =  "sha256-SUucXy6wis2kONtdDHMVJL3MK1OZ6RxMfumK8rxGlXI=";
      };
      image = "bbilly1/tubearchivist:v0.3.6";

      ports = [ "8000:8000" ];

      volumes = [
        "${media}:/youtube"
        "${cache}:/cache" 
      ];

      environment = {
        ES_URL = "http://archivist-es:9200";
        REDIS_HOST = "archivist-redis";
        HOST_UID = "1000";
        HOST_GID = "1000";
        TA_HOST = "tubearchivist.service localhost";
        TA_USERNAME = "solomon";
        TA_PASSWORD = "hunter2";
        ELASTIC_PASSWORD = "hunter2";
        TZ = "America/Los_Angeles";
      };

      dependsOn = [ "archivist-es" "archivist-redis" ];

      extraOptions = [
        "--network=tubearchivist-br"
      ];
    };

    archivist-es = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName =  "bbilly1/tubearchivist-es";
        imageDigest =  "sha256:861409a3479ce337308777570ea6caa179731c59e10181d2931a110766b0d2fb"; 
        sha256 =  "sha256-Qd0I9q9pQF1EO41dWkwTpPCIEsMHgga0jWcq1VmKpnA=";
      };
      image = "bbilly1/tubearchivist-es:8.7.0";

      environment = {
        ELASTIC_PASSWORD = "hunter2";
        ES_JAVA_OPTS = "-Xms512m -Xmx512m";
        "xpack.security.enabled" = "true";
        "discovery.type" = "single-node";
        "path.repo" = "/usr/share/elasticsearch/data/snapshot";
      };

      ports = [ "9200:9200" ];

      volumes = [
        "es:/usr/share/elasticsearch/data"
      ];

      extraOptions = [
        "--network=tubearchivist-br"
      ];

      autoStart = true;
    };

    archivist-redis = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName =  "redis/redis-stack-server";
        imageDigest = "sha256:e2a73d78daf3fb2053e0d1555c0b041ffc8d7cd0fcfa5a045679fad3245a4260"; 
        sha256 =  "sha256-S8Z7kTEOa0bosGcLoYmyvopz3z/Tis9ZzBAUn2o2Rkk=";
      };
      image = "redis/redis-stack-server:latest";

      ports = [ "6379:6379" ];

      volumes = [
        "es:/usr/share/elasticsearch/data"
      ];

      #dependsOn = [ "archivist-es" ];

      extraOptions = [
        "--network=tubearchivist-br"
      ];

      autoStart = true;
    };

  };

  systemd.services.init-tubearchivist-bridge = {
    description = "Create the network bridge for tubearchivist.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script =
      let dockercli = "${config.virtualisation.docker.package}/bin/docker";
      in ''
        # Put a true at the end to prevent getting non-zero return code, which will
        # crash the whole service.
        check=$(${dockercli} network ls | grep "tubearchivist-br" || true)
        if [ -z "$check" ]; then
          ${dockercli} network create tubearchivist-br 
        else
          echo "tubearchivist-br already exists in docker"
        fi
      '';
  };
  
  services.nginx.virtualHosts."tubearchivist.service" = {
    locations."/".proxyPass = "http://localhost:8000";
  };
}
