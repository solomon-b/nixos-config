{ config, pkgs, ... }:

let
  media = "/mnt/tubearchivist/media";
  cache = "/mnt/tubearchivist/cache";
in
{
  fileSystems."/mnt/tubearchivist/media" = {
    device = "192.168.5.6:/mnt/tank/app-data/tube-archivist/media";
    fsType = "nfs";
  };

  fileSystems."/mnt/tubearchivist/cache" = {
    device = "192.168.5.6:/mnt/tank/app-data/tube-archivist/cache";
    fsType = "nfs";
  };

  virtualisation.oci-containers.containers = {
    tubearchivist = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "bbilly1/tubearchivist";
        imageDigest = "sha256:589dbfcd7ba36e608294cc586b80820a6651eaa80cc22eba871aa9980cdc85fd";
        sha256 = "sha256-kREVFRdayfeSgVaxaBKBRmUTXdkFA2ddI06MTBlaChY=";
      };
      image = "bbilly1/tubearchivist:v0.4.5";

      ports = [ "8000:8000" ];

      volumes = [
        "${media}:/youtube"
        "${cache}:/cache"
      ];

      environment = {
        ES_URL = "http://archivist-es:9200";
        REDIS_HOST = "archivist-redis";
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
       autoStart = true;
    };

    archivist-es = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "bbilly1/tubearchivist-es";
        imageDigest = "sha256:152ed8f62d80725ef0d2338765b3b5d8c87f5e6f9b70d864980059616eb4a2ff";
        sha256 = "sha256-XpnbnUl75If/gXqSGznhTZTUaBZF4PoQimt6qIcNUxI=";
      };
      image = "bbilly1/tubearchivist-es:8.13.2";

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
        imageName = "redis/redis-stack-server";
        imageDigest = "sha256:7df84d4e2f0e1d3d5d85f6ee96f1a42effe851527a72170933f8822341f83b74";
        sha256 = "sha256-yeT9lhX1ArzVUyRG50a01PJn8Kifv7HPORzEf7DtT5c=";
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

  services.nginx.virtualHosts."tubearchivist.local" = {
    locations."/".proxyPass = "http://localhost:8000";
  };
}
