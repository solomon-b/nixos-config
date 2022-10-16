{ ... }:

{
  services.airsonic = {
    enable = true;
  };

  services.nginx.virtualHosts."airsonic.sower" = {
    locations."/".proxyPass = "http://localhost:4040";
  };
}
