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

  systemd.services.coredns = {
    after = [ "tailscaled.service" ];
    wants = [ "tailscaled.service" ];
  };

  services.coredns = {
    enable = true;
    config = ''
      . {
          bind 192.168.5.100 100.117.45.47

          cache 30
          errors
          log

          forward . 1.1.1.1 1.0.0.1 {
              except home.arpa
          }

          view tailscale {
              expr incidr(client_ip(), '100.64.0.0/10')
          }
          hosts {
              # Local IPs (default for non-Tailscale clients)
              192.168.5.6  sandra-voi.home.arpa
              192.168.5.104 apollyon

              # Tailscale IPs
              100.123.147.26 filebrowser.service.home.arpa
              100.123.147.26 homebox.service.home.arpa
              100.123.147.26 homepage.service.home.arpa
              100.123.147.26 qbittorrent.service.home.arpa
              100.123.147.26 bazarr.service.home.arpa
              100.123.147.26 lidarr.service.home.arpa
              100.123.147.26 prowlarr.service.home.arpa
              100.123.147.26 radarr.service.home.arpa
              100.123.147.26 sonarr.service.home.arpa
              100.123.147.26 sabnzbd.service.home.arpa
              100.123.147.26 planka.service.home.arpa
              100.123.147.26 paperless.service.home.arpa
              100.80.98.4 jellyfin.service.home.arpa
              100.80.98.4 jellyseerr.service.home.arpa
              100.80.98.4 podgrab.service.home.arpa
              100.80.98.4 navidrome.service.home.arpa
              100.80.98.4 immich.service.home.arpa
              100.80.98.4 tubearchivist.service.home.arpa
              100.80.98.4 home-assistant.service.home.arpa
              100.80.98.4 zigbee2mqtt.service.home.arpa
              100.80.98.4 soulseek.service.home.arpa
              100.97.232.9 uptime.service.home.arpa
              fallthrough
          }
      }

      . {
          bind 192.168.5.100 100.117.45.47

          cache 30
          errors
          log

          forward . 1.1.1.1 1.0.0.1 {
              except home.arpa
          }

          hosts {
              # Local IPs (default for non-Tailscale clients)
              192.168.5.6  sandra-voi.home.arpa
              192.168.5.104 apollyon
              192.168.5.105 filebrowser.service.home.arpa
              192.168.5.105 homebox.service.home.arpa
              192.168.5.105 homepage.service.home.arpa
              192.168.5.105 qbittorrent.service.home.arpa
              192.168.5.105 bazarr.service.home.arpa
              192.168.5.105 lidarr.service.home.arpa
              192.168.5.105 prowlarr.service.home.arpa
              192.168.5.105 radarr.service.home.arpa
              192.168.5.105 sonarr.service.home.arpa
              192.168.5.105 sabnzbd.service.home.arpa
              192.168.5.105 planka.service.home.arpa
              192.168.5.105 paperless.service.home.arpa
              192.168.5.7 jellyfin.service.home.arpa
              192.168.5.7 jellyseerr.service.home.arpa
              192.168.5.7 podgrab.service.home.arpa
              192.168.5.7 navidrome.service.home.arpa
              192.168.5.7 immich.service.home.arpa
              192.168.5.7 tubearchivist.service.home.arpa
              192.168.5.7 home-assistant.service.home.arpa
              192.168.5.7 zigbee2mqtt.service.home.arpa
              192.168.5.7 soulseek.service.home.arpa
              192.168.5.103 uptime.service.home.arpa
              fallthrough
          }
      }
    '';
  };
}
