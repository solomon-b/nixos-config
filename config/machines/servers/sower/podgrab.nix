{ ... }:


{
  virtualisation.oci-containers.containers.podgrab = {
    image = "akhilrex/podgrab:latest";
    ports = [ "8081:8080"];
    volumes = [
      "/mnt/media/Podcasts:/assets"
    ];
    environment =  {
      CHECK_FREQUENCY = "240";
    };
  };

  services.nginx.virtualHosts."podgrab.service" = {
    locations."/".proxyPass = "http://localhost:8081";
  };

  networking.firewall.allowedTCPPorts = [ 8081 ];
}
