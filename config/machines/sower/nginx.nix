{ ... }:

{
  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
  };

  # 8080 == heimdall
  # 8081 == qBitTorrent
  # 8082 == FreshRSS
  # 8083 == Hoogle
  # 8096 == Jellyfin
  # 2342 == Photoprism

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx.virtualHosts = {
    "nas.sower" = {
      locations."/" = {
        proxyPass = "http://192.168.1.174";
        extraConfig = ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection $http_connection;
        '';
      };
    };

    "nextcloud.sower" = {
      locations."/".proxyPass = "https://192.168.1.61";
    };
  };
}
