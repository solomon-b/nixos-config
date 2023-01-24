# nginx reverse proxy and docker orchestration
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix

    ./heimdall.nix
    ./homepage.nix
    ./filebrowser.nix
    ./tt-rss.nix
    ../../../profiles/virtual-machine
    ../../../modules/services/docker
  ];

  networking.hostName = "accompaniment-of-shadows";

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
  };
  
  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx.virtualHosts = {
    "qbittorrent.local" = {
      locations."/" = {
        proxyPass = "http://192.168.5.104:8081";
      };
    };
  };
}
