{ ... }:

{
  services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      virtualHosts = {
        "sower" = {
          root = "/srv/www/sower.galaxybrain.zone";
          locations = {
            "/jellyfin" = {
              proxyPass = "http://100.80.98.4:8096";
            };
            "/qbittorrent" = {
              proxyPass = "http://100.80.98.4:8080";
            };
            "/syncthing" = {
              proxyPass = "http://100.80.98.4:8384";
            };
          };
        };
      };
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
