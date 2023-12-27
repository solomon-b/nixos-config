{ ... }:

{
  virtualisation.oci-containers.containers.homepage = {
    image = "ghcr.io/benphelps/homepage:latest";
    ports = [ "3000:3000" ];
    volumes = [
      "/etc/homepage:/app/config"
    ];
  };

  services.nginx.virtualHosts."homepage.service" = {
    locations."/".proxyPass = "http://localhost:3000";
  };
}
