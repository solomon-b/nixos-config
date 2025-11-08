{ pkgs, ... }:

{
  imports = [
    ./hardware.nix

    ./filebrowser.nix
    ./homebox.nix
    ./homepage.nix
    ./observability.nix
    ./paperless-ngx.nix
    ./planka.nix
    ./sabnzbd.nix
    ./servarr.nix

    ../../../profiles/virtual-machine
  ];

  networking = {
    hostName = "accompaniment-of-shadows";
    firewall.allowedTCPPorts = [ 80 8080 ];
  };

  services = {
    nginx = {
      enable = true;

      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
    };

    nginx.virtualHosts = {
      "qbittorrent.service.home.arpa" = {
        locations."/" = {
          proxyPass = "http://192.168.5.104:8081";
        };
      };
    };
  };

  virtualisation = {
    containers = {
      enable = true;
    };

    docker = {
      enable = true;
      storageDriver = "overlay2";
    };
    oci-containers.backend = "docker";
  };

  primary-user.extraGroups = [ "docker" ];
}
