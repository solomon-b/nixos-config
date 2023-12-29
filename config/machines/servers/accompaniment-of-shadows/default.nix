# nginx reverse proxy and docker orchestration
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix

    ./filebrowser.nix
    ./homepage.nix
    ./sabnzbd.nix
    ./servarr.nix

    ../../../profiles/virtual-machine
  ];

  networking.hostName = "accompaniment-of-shadows";

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
  };

  networking.firewall.allowedTCPPorts = [ 80 8080 ];

  services.nginx.virtualHosts = {
    "qbittorrent.service" = {
      locations."/" = {
        proxyPass = "http://192.168.5.104:8081";
      };
    };

    "lidarr.service" = {
      locations."/" = {
        proxyPass = "http://localhost:8686";
      };
    };

    "prowlarr.service" = {
      locations."/" = {
        proxyPass = "http://localhost:9696";
      };
    };

    "sabnzbd.service" = {
      locations."/" = {
        proxyPass = "http://localhost:8080";
      };
    };
  };

  virtualisation = {
    containers = {
      enable = true;
    };

    docker = {
      enable = true;
      storageDriver = "devicemapper";
    };
    oci-containers.backend = "docker";
  };

  primary-user.extraGroups = [ "docker" ];
}
