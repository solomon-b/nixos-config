{ pkgs, ... }:

{
  services.qBittorrent = {
    enable = true;
    webUIAddress.port = 8081;
  };

  users.users.qBittorrent.extraGroups = [ "nas" ];

  services.nginx.virtualHosts."qbittorrent.sower" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:8081";
    };
  };
}
