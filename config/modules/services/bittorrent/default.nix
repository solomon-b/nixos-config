{ pkgs, ... }:

{
  services.qBittorrent = {
    enable = true;
  };

  users.users.qBittorrent.extraGroups = [ "nas" ];

  services.nginx.virtualHosts."qbittorrent.sower" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:8080";
      extraConfig =
        "proxy_set_header Host 127.0.0.1:8080;" +
        "proxy_set_header X-Forwarded-Host $http_host;" +
        "proxy_set_header X-Forwarded-For $remote_addr;";
    };
  };
}
