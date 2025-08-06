# pi-hole vm
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
  ];

  networking = {
    hostName = "silence-under-snow";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };

  networking.firewall.allowedTCPPorts = [ 53 4000 ];
  networking.firewall.allowedUDPPorts = [ 53 ];

  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  services.blocky = {
    enable = true;
    settings = {
      upstream = {
        default = [ "1.1.1.1" "1.0.0.1" ];
      };

      port = 53;
      httpPort = 4000;

      customDNS.mapping = {
        # Physical Machines
        "zodiacal-light.home.arpa" = "100.76.234.131";
        "nightshade.home.arpa" = "100.72.120.112";
        "lorean.home.arpa" = "100.126.171.15";
        "sower.home.arpa" = "100.80.98.4";
        "sandra-voi.home.arpa" = "192.168.5.6";
        "voice-of-evening.home.arpa" = "100.110.60.76";

        # Virtual Machines on sandra-voi
        "accompaniment-of-shadows.home.arpa" = "100.123.147.26";
        "apollyon" = "192.168.5.104";
        "apollyon.home.arpa" = "192.168.5.104";
        "madonna-of-the-wasps.home.arpa" = "100.70.16.79";
        "silence-under-snow.home.arpa" = "100.117.45.47";
        "storm-bird.home.arpa" = "100.97.232.9";
        "transfigured-night.home.arpa" = "100.96.251.72";

        # Services on accompaniment-of-shadows
        "filebrowser.service.home.arpa" = "100.123.147.26";
        "homebox.service.home.arpa" = "100.123.147.26";
        "homepage.service.home.arpa" = "100.123.147.26";
        "qbittorrent.service.home.arpa" = "100.123.147.26";
        "bazarr.service.home.arpa" = "100.123.147.26";
        "lidarr.service.home.arpa" = "100.123.147.26";
        "prowlarr.service.home.arpa" = "100.123.147.26";
        "radarr.service.home.arpa" = "100.123.147.26";
        "sonarr.service.home.arpa" = "100.123.147.26";
        "sabnzbd.service.home.arpa" = "100.123.147.26";
        "planka.service.home.arpa" = "100.123.147.26";
        "paperless.service.home.arpa" = "100.123.147.26";
        "planka.local.home.arpa" = "192.168.5.105";
        "paperless.local.home.arpa" = "192.168.5.105";

        # Services on sower
        "jellyfin.local.home.arpa" = "192.168.5.7";
        "jellyfin.service.home.arpa" = "100.80.98.4";
        "jellyseerr.local.home.arpa" = "192.168.5.7";
        "jellyseerr.service.home.arpa" = "100.80.98.4";
        "podgrab.service.home.arpa" = "100.80.98.4";
        "navidrome.service.home.arpa" = "100.80.98.4";
        "immich.service.home.arpa" = "100.80.98.4";
        "tubearchivist.local.home.arpa" = "192.168.5.7";
        "tubearchivist.service.home.arpa" = "100.80.98.4";
        "home-assistant.service.home.arpa" = "100.80.98.4";
        "home-assistant.local.home.arpa" = "192.168.5.7";
        "zigbee2mqtt.service.home.arpa" = "100.80.98.4";
        "soulseek.service.home.arpa" = "100.80.98.4";

        # Services on storm-bird
        "uptime.service.home.arpa" = "100.97.232.9";
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
        path = "/metrics";
      };
    };
  };
}
