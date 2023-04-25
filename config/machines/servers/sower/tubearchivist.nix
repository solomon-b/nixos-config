{ config, pkgs, ... }:

let
  media = "/mnt/tubearchivist/media";
  cache = "/mnt/tubearchivist/cache";

  redisHostname = "172.21.2.3";
  esHostname = "172.21.2.4";
  esPort = "9200";
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

  virtualisation.oci-containers.containers.tubearchivist = {
    image = "bbilly1/tubearchivist:latest";

    environment = {
      ES_URL = "http://${esHostname}:${esPort}";
      REDIS_HOST = redisHostname;
      HOST_UID = "1000";
      HOST_GID = "1000";
      TA_HOST = "tubearchivist.service";
      TA_PASSWORD = "hunter2";
      ELASTIC_PASSWORD = "hunter2";
      TZ = "America/Los_Angeles";
    };

    volumes = [
      "${media}:/youtube"
      "${cache}:/cache" 
    ];

    dependsOn = [ "tubearchivist-es" ];

    extraOptions = [
      "--network=tubearchivist-br"
      "--ip=172.21.2.2"
    ];
  };

  virtualisation.oci-containers.containers.tubearchivist-redis = {
    image = "redis/redis-stack-server:latest";

    volumes = [
      "es:/usr/share/elasticsearch/data"
    ];

    dependsOn = [ "tubearchivist-es" ];

    extraOptions = [
      "--network=tubearchivist-br"
      "--ip=172.21.2.3"
    ];

    ports = [ "6379:6379" ];
  };

  virtualisation.oci-containers.containers.tubearchivist-es = {
    image = "bbilly1/tubearchivist-es:latest";

    environment = {
      ELASTIC_PASSWORD = "hunter2";
      ES_JAVA_OPTS = "-Xms512m -Xmx512m";
      "xpack.security.enabled" = "true";
      "discovery.type" = "single-node";
      "path.repo" = "/usr/share/elasticsearch/data/snapshot";
    };

    volumes = [
      "es:/usr/share/elasticsearch/data"
    ];

    dependsOn = [ "tubearchivist-es" ];

    extraOptions = [
      "--network=tubearchivist-br"
      "--ip=172.21.2.4"
    ];
  };

  systemd.services.init-tubearchivist-bridge = {
    description = "Create the network bridge for tubearchivist.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    
    serviceConfig.Type = "oneshot";
     script = let dockercli = "${config.virtualisation.docker.package}/bin/docker";
             in ''
               # Put a true at the end to prevent getting non-zero return code, which will
               # crash the whole service.
               check=$(${dockercli} network ls | grep "tubearchivist-br" || true)
               if [ -z "$check" ]; then
                 ${dockercli} network create \
                   --driver=bridge \
                   --subnet=172.21.2.0/24 \
                   --gateway=172.21.2.1 \
                   -o "com.docker.network.bridge.enable_ip_masquerade"="true" \
                   -o "com.docker.network.bridge.enable_icc"="true" \
                   -o "com.docker.network.driver.mtu"="1500" \
                   tubearchivist-br 
               else
                 echo "tubearchivist-br already exists in docker"
               fi
             '';
  };
  
  services.nginx.virtualHosts."tubearchivist.service" = {
    locations."/".proxyPass = "http://172.21.2.2:8000";
  };
}
