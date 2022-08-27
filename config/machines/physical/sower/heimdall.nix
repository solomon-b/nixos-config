{ ... }:

{
  virtualisation.oci-containers.containers.heimdall = {
    image = "linuxserver/heimdall";
    ports = [ "8080:80" "406:443" ];
    volumes = [
      "/srv/www/heimdall:/config"
    ];
    environment = {
      PUID="1000";
      PGID="1000";
      TZ = "America/Los_Angeles";
    };
  };

  services.nginx.virtualHosts."sower" = {
    locations."/".proxyPass = "http://localhost:8080";
  };
}
