{ config, ... }:

{
  services.sabnzbd = {
    enable = true;
  };

  services.nginx.virtualHosts = {
    "sabnzbd.service.home.arpa" = {
      locations."/" = {
        proxyPass = "http://localhost:8080";
      };
    };
  };
}
