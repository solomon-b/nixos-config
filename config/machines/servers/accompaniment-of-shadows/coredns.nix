{ ... }:

{
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  systemd.services.coredns = {
    after = [ "tailscaled.service" ];
    wants = [ "tailscaled.service" ];
  };

  services.coredns = {
    enable = true;
    config = ''
      # Block 1: Tailscale clients (queries to 100.123.147.26)
      .:53 {
          bind 100.123.147.26

          cache 30
          errors
          log

          forward . 1.1.1.1 1.0.0.1 {
              except home.arpa
          }

          hosts {
              # Machines without Tailscale (accessed via subnet route)
              192.168.5.6   sandra-voi.home.arpa
              192.168.5.104 apollyon

              # Services on accompaniment-of-shadows (Tailscale IP)
              100.123.147.26 filebrowser.service.home.arpa
              100.123.147.26 homebox.service.home.arpa
              100.123.147.26 homepage.service.home.arpa
              100.123.147.26 lubelogger.service.home.arpa
              100.123.147.26 qbittorrent.service.home.arpa
              100.123.147.26 bazarr.service.home.arpa
              100.123.147.26 lidarr.service.home.arpa
              100.123.147.26 prowlarr.service.home.arpa
              100.123.147.26 profilarr.service.home.arpa
              100.123.147.26 radarr.service.home.arpa
              100.123.147.26 sonarr.service.home.arpa
              100.123.147.26 sabnzbd.service.home.arpa
              100.123.147.26 planka.service.home.arpa
              100.123.147.26 paperless.service.home.arpa
              100.123.147.26 grafana.service.home.arpa
              100.123.147.26 uptime.service.home.arpa

              # Services on sower (Tailscale IP)
              100.80.98.4 jellyfin.service.home.arpa
              100.80.98.4 jellyseerr.service.home.arpa
              100.80.98.4 podgrab.service.home.arpa
              100.80.98.4 navidrome.service.home.arpa
              100.80.98.4 immich.service.home.arpa
              100.80.98.4 tubearchivist.service.home.arpa
              100.80.98.4 home-assistant.service.home.arpa
              100.80.98.4 zigbee2mqtt.service.home.arpa
              100.80.98.4 soulseek.service.home.arpa
              100.80.98.4 wikipedia.service.home.arpa

              fallthrough
          }
      }

      # Block 2: LAN clients (queries to 192.168.5.105)
      .:53 {
          bind 192.168.5.105

          cache 30
          errors
          log

          forward . 1.1.1.1 1.0.0.1 {
              except home.arpa
          }

          hosts {
              # Machines on LAN
              192.168.5.6   sandra-voi.home.arpa
              192.168.5.104 apollyon

              # Services on accompaniment-of-shadows (local IP)
              192.168.5.105 filebrowser.service.home.arpa
              192.168.5.105 homebox.service.home.arpa
              192.168.5.105 homepage.service.home.arpa
              192.168.5.105 lubelogger.service.home.arpa
              192.168.5.105 qbittorrent.service.home.arpa
              192.168.5.105 bazarr.service.home.arpa
              192.168.5.105 lidarr.service.home.arpa
              192.168.5.105 prowlarr.service.home.arpa
              192.168.5.105 profilarr.service.home.arpa
              192.168.5.105 radarr.service.home.arpa
              192.168.5.105 sonarr.service.home.arpa
              192.168.5.105 sabnzbd.service.home.arpa
              192.168.5.105 planka.service.home.arpa
              192.168.5.105 paperless.service.home.arpa
              192.168.5.105 grafana.service.home.arpa
              192.168.5.105 uptime.service.home.arpa

              # Services on sower (local IP)
              192.168.5.7 jellyfin.service.home.arpa
              192.168.5.7 jellyseerr.service.home.arpa
              192.168.5.7 podgrab.service.home.arpa
              192.168.5.7 navidrome.service.home.arpa
              192.168.5.7 immich.service.home.arpa
              192.168.5.7 tubearchivist.service.home.arpa
              192.168.5.7 home-assistant.service.home.arpa
              192.168.5.7 zigbee2mqtt.service.home.arpa
              192.168.5.7 soulseek.service.home.arpa
              192.168.5.7 wikipedia.service.home.arpa

              fallthrough
          }
      }
    '';
  };
}
