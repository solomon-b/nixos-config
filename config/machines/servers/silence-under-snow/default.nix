# pi-hole vm
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
    ../../../modules/services/docker
  ];

  networking = {
    hostName = "silence-under-snow";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];

  services.blocky = {
    enable = true;
    settings = {
      upstream = {
        default = [ "1.1.1.1" "1.0.0.1" ];
      };

      port = 53;
      httpPort = 4000;

      customDNS.mapping = {
        "nightshade" = "100.100.33.33";
        "nightshade.machine" = "100.100.33.33";
        "lorean" = "100.92.19.49";
        "lorean.machine" = "100.92.19.49";
        "sower" = "100.80.98.4";
        "sower.machine" = "100.80.98.4";
        "sandra-voi" = "192.168.5.6";
        "sandra-voi.machine" = "192.168.5.6";

        "accompaniment-of-shadows" = "100.123.147.26";
        "apollyon" = "192.168.5.104";
        "madonna-of-the-wasps" = "100.70.16.79";
        "silence-under-snow" = "100.117.45.47";
        "storm-bird" = "100.97.232.9";
        "transfigured-night" = "100.96.251.72";

        "filebrowser.service" = "100.123.147.26";
        "heimdall.service" = "100.123.147.26";
        "homepage.service" = "100.123.147.26";
        "qbittorrent.service" = "100.123.147.26";
        "lidarr.service" = "100.123.147.26";
        "prowlarr.service" = "100.123.147.26";
        "sabnzbd.service" = "100.123.147.26";

        "jellyfin.local" = "192.168.5.7";
        "jellyfin.service" = "100.80.98.4";
        "podgrab.service" = "100.80.98.4";
        "navidrome.service" = "100.80.98.4";
        "immich.service" = "100.80.98.4";
      };

      blocking = {
        blackLists = {
          ads = [
            "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt"
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
          ];
        };
        clientGroupsBlock = {
          default = [ "ads" ];
        };
      };

      prometheus = {
        enable = true;
        path =  "/metrics";
      };
    };
  };
}
