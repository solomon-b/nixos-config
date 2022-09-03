# nginx reverse proxy
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix

    ./heimdall.nix
    ./filebrowser.nix
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
    "qbittorrent2.local" = {
      locations."/" = {
        proxyPass = "http://192.168.1.137:8081";
      };
    };
  };
}
