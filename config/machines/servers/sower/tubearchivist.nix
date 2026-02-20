{ config, pkgs, ... }:

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
    tubearchivist = {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = "bbilly1/tubearchivist";
        imageDigest = "sha256:b827a713f55b2b1933f8b130f36dc8b38cec3dd56998c389b7c9694a7238f3df";
        sha256 = "sha256-RGcPXK9DmByxxjcq5Um7UeEbRuLW6x02r9MAc+ZVciU=";
      };
      image = "bbilly1/tubearchivist:v0.5.9";

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
        imageName = "redis";
        imageDigest = "sha256:7b6fb55d8b0adcd77269dc52b3cfffe5f59ca5d43dec3c90dbe18aacce7942e1";
        sha256 = "sha256-yb2pRkQVgb7uI26rUYDz7e4Nekp3dj+RFAODG55XbeU=";
      };
      image = "redis:latest";

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
