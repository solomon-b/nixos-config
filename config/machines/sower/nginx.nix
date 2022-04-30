{ ... }:

{
  services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      virtualHosts = {
        "qbittorrent.sower" =  {
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
          };
        };

        "jellyfin.sower" =  {
          locations."/" = {
            proxyPass = "http://127.0.0.1:8096";
          };
        };

        "${fqdn}" = {
          forceSSL = true;
          enableACME = true;
          root = "/srv/www/sower.galaxybrain.zone";
        };
      };
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
