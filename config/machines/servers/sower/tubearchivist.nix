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
      image = "bbilly1/tubearchivist:v0.6.0";
      imageDigest = "sha256:0cbc18aa953c72ec7dfdd6f8fa7c2fe89b5d04489857d7baa354fecbd6181e69";
      sha256 = "sha256-L3HleF9v9zcgO8qhzlGZFi8bG9KpesdiKd6ww/+woDk=";

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
      image = "bbilly1/tubearchivist-es:9.5.5";
      imageDigest = "sha256:9513a22cd332402cb6fd6ef69844f4f78717190ba9e2e08cb72b3288839d84fa";
      sha256 = "sha256-4lW5/75v01Kz8LlVZ5A60uvC/Nufsr3p5F6fH+K+YJU=";

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
      image = "redis:8.10.3";
      imageDigest = "sha256:1f88291b4c57bdf249f02eb085a8669889b2d2954e91d4de5118a5da6b78dd53";
      sha256 = "sha256-HrVR03KEkZ8/DtTJpMkAeodeuorVu60PQbQNZP5PkE4=";

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
