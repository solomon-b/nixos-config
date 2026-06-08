{ config, mkPin, ... }:

let
  media = "/mnt/tubearchivist/media";
  cache = "/mnt/tubearchivist/cache";
in
{
  fileSystems."/mnt/tubearchivist/media" = {
    device = "192.168.5.6:/mnt/tank/app-data/tube-archivist/media";
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

  fileSystems."/mnt/tubearchivist/cache" = {
    device = "192.168.5.6:/mnt/tank/app-data/tube-archivist/cache";
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
    tubearchivist = mkPin {
      image = "bbilly1/tubearchivist:v0.5.10";
      imageDigest = "sha256:dfe723cf008520e1758ecc3e59e6ea8761dd10d5bb099cd87289e80f5bd66567";
      sha256 = "sha256-VXvBggbz6vEFXW/CYtunG74wHMZzDGrrHNnoQyqGtDY=";

      ports = [ "8000:8000" ];

      volumes = [
        "${media}:/youtube"
        "${cache}:/cache"
      ];

      environment = {
        ES_URL = "http://archivist-es:9200";
        REDIS_CON = "redis://archivist-redis:6379";
        TA_HOST = "http://tubearchivist.service.home.arpa";
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

    archivist-es = mkPin {
      image = "bbilly1/tubearchivist-es:8.19.0";
      imageDigest = "sha256:9da63fb1973ec3d57daf6916be948eddd0d8a404cc8e447c938480c85fe2c554";
      sha256 = "sha256-shRcHXF7zh1ApOwDntbqBlUgjGmMNZSvQNl/QI7zOLM=";

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

    archivist-redis = mkPin {
      image = "redis:7.4.9";
      imageDigest = "sha256:d3be87a1060455213a204d2b0a7f04d45d19a16a98e85b3c37b7c33b5f0c489e";
      sha256 = "sha256-9lk+a33Xqrdm14Gk7avX7tx+UUpFi3hQvCN/jGwrtgc=";

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

  services.nginx.virtualHosts."tubearchivist.service.home.arpa" = {
    locations."/".proxyPass = "http://localhost:8000";
  };
}
