{ pkgs, config, ... }:

{
  services.nginx.virtualHosts."nextcloud.sower" = {
    locations."/".proxyPass = "http://172.21.1.3:80";
  };

  virtualisation.oci-containers.containers.nextcloud = {
    image = "nextcloud";
    autoStart = true;

    volumes = [
      "/srv/NAS/nextcloud:/var/www/html"
    ];

    environment = {
      TZ = "America/Los_Angeles";
      MYSQL_PASSWORD = "hunter2";
      MYSQL_DATABASE = "nextcloud";
      MYSQL_USER = "nextcloud";
      MYSQL_HOST = "172.21.1.2";
      MYSQL_PORT = "3306";
    };

    extraOptions = [
      "--network=nextcloud-br"
      "--ip=172.21.1.3"
    ];
  };

  virtualisation.oci-containers.containers.mariadb-nextcloud = {
    image = "mariadb:10.5";
    autoStart = true;

    cmd = [
      "--transaction-isolation=READ-COMMITTED"
      "--binlog-format=ROW"
    ];

    volumes = [
      "/srv/NAS/db:/var/lib/mysql"
    ];

    extraOptions = [
      "--network=nextcloud-br"
      "--ip=172.21.1.2"
    ];

    environment = {
      MYSQL_ROOT_PASSWORD = "hunter2";
      MYSQL_PASSWORD = "hunter2";
      MYSQL_DATABASE = "nextcloud";
      MYSQL_USER = "nextcloud";
    };
  };

  systemd.services.init-nextcloud-bridge = {
    description = "Create the network bridge for nextcloud.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    
    serviceConfig.Type = "oneshot";
     script = let dockercli = "${config.virtualisation.docker.package}/bin/docker";
             in ''
               # Put a true at the end to prevent getting non-zero return code, which will
               # crash the whole service.
               check=$(${dockercli} network ls | grep "nextcloud-br" || true)
               if [ -z "$check" ]; then
                 ${dockercli} network create \
                   --driver=bridge \
                   --subnet=172.21.1.0/24 \
                   --gateway=172.21.1.1 \
                   -o "com.docker.network.bridge.enable_ip_masquerade"="true" \
                   -o "com.docker.network.bridge.enable_icc"="true" \
                   -o "com.docker.network.driver.mtu"="1500" \
                   nextcloud-br 
               else
                 echo "nextcloud-br already exists in docker"
               fi
             '';
  };

  systemd.services.docker-nextcloud = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
  };

  systemd.services.docker-mariadb-nextcloud = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
  };
}
