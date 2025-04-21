{ config, pkgs, ... }:

{
  services.homepage-dashboard = {
    enable = true;
    listenPort = 3000;
    allowedHosts = "*";
    environmentFile = "/etc/homepage.env";

    settings = {
      layout = {
        Organization = {
          style = "row";
          columns = 2;  
        };
      };
    };

    bookmarks = [
      {
        KPBJ = [
          {
            "Mailcheap Portal" = [{
              abbr = "MP";
              href = "https://mail10.mymailcheap.com/web/login.html";
            }];
          }
        ];
      }
      {
        Work = [
          { Gusto = [{
             abbr = "G";
             href = "https://app.gusto.com/";
            }];
          }
          {
            Justworks = [{
              abbr = "JW";
              href = "https://secure.justworks.com/dashboard";
            }];
          }
          {
            # Bitnomial 401k
            Guideline = [{
              abbr = "GL";
              href = "https://my.guideline.com";
            }];
          }
          {
            # Co-Star 401k
            Empower-Retirement = [{
              abbr = "ER";
              href = "https://justworks.empower-retirement.com";
            }];
          }
          {
            # CJ 401k
            Fidelity = [{
              abbr = "FI";
              href = "https://digital.fidelity.com/ftgw/digital/portfolio/summary#08063";
            }];
          }
          {
            Hasura-Rippling = [{
              abbr = "RP";
              href = "https://app.rippling.com/dashboard";
            }];
          }
          {
            Mortgage-Capital-Partners = [{
              abbr = "MCP";
              href = "https://www.yourmortgageonline.com/payments";
            }];
          }
        ];
      }
      {
        Homelab = [
          {
            Tailscale = [{
              abbr = "TS";
              href = "https://login.tailscale.com/admin/machines";
            }];
          }

          {
            "Digital Ocean" = [{
              abbr = "DO";
              href = "https://cloud.digitalocean.com/projects";
            }];
          }
          {
            Cloudflare = [{
              abbr = "CF";
              href = "https://dash.cloudflare.com";
            }];
          }
        ];
      }
      {
        Social = [
          {
            "Cofree Chat" = [{
              abbr = "CC";
              href = "https://riot.cofree.coffee";
            }];
          }
          {
            Reddit = [{
              abbr = "RE";
              href = "https://old.reddit.com/";
            }];
          }
          {
            "Hacker News" = [{
              abbr = "HN";
              href = "https://news.ycombinator.com";
            }];
          }
          {
            Lobsters = [{
              abbr = "L";
              href = "https://lobste.rs";
            }];
          }
          {
            YouTube = [{
              abbr = "YT";
              href = "https://youtube.com/";
            }];
          }
          {
            "Last.fm" = [{
              abbr = "FM";
              href = "https://last.fm";
            }];
          }
          {
            "Listen Brainz" = [{
              abbr = "LB";
              href = "https://listenbrainz.org/user/solomon-b";
            }];
          }
        ];
      }
    ];

    services = [
      {
        Organization = [
          {
            Paperless = {
              href = "http://paperless.service";
              icon = "paperless";
              widget = {
                type = "paperless";
                url = "http://paperless.service";
                username = "admin";
                password = "{{HOMEPAGE_FILE_PAPERLESS_PASSWORD}}";
              };
            };
          }
          {
            Planka = {
              href = "http://planka.service";
            };
          }
          {
            Homebox = {
              href = "http://homebox.service";
              icon = "homebox";
              widget = {
                type = "homebox";
                url = "http://homebox.service";
                username = "ssbothwell@gmail.com";
                password = "{{HOMEPAGE_FILE_HOMEBOX_PASSWORD}}";
              };
            };
          }
          {
            Home-Assistant = {
              href = "http://home-assistant.service";
              icon = "home-assistant";
              widget = {
                type = "homeassistant";
                url = "http://home-assistant.service";
                key = "{{HOMEPAGE_FILE_HOMEASSISTANT_KEY}}";
                custom = [
                  {
                    state = "sensor.envoy_202202063519_current_power_production";
                    label = "energy today";
                  }
                  {
                    state = "weather.forecast_home";
                    label = "wind speed";
                    value = "{attributes.wind_speed} {attributes.wind_speed_unit}";
                  }
                ];
              };
            };
          }
          {
            Planka = {
              href = "http://planka.service";
            };
          }
        ];
      }
      {
        Media = [
          {
            Jellyseerr = {
              href = "http://jellyseerr.service";
              icon = "jellyseerr";
              widget = {
                type = "jellyseerr";
                url = "http://jellyseerr.service";
                key = "{{HOMEPAGE_FILE_JELLYSEER_KEY}}";
              };
            };
          }
          {
            Jellyfin = {
              href = "http://jellyfin.service";
              icon = "jellyfin";
              widget = {
                type = "jellyfin";
                url = "http://jellyfin.service";
                key = "{{HOMEPAGE_FILE_JELLYFIN_KEY}}";
              };
            };
          }
          {
            navidrome = {
              href = "http://navidrome.service";
              icon = "navidrome";
              widget = {
                type = "navidrome";
                url = "http://navidrome.service";
                user = "admin";
                token = "{{HOMEPAGE_FILE_NAVIDROME_TOKEN}}";
                salt = "{{HOMEPAGE_FILE_NAVIDROME_SALT}}";
              };
            };
          }
          {
            Podgrab = {
              href = "http://podgrab.service";
              description = "Podcast Management";
            };
          }
          {
            immich = {
              href = "http://immich.service";
              icon = "immich";
              widget = {
                type = "immich";
                url = "http://immich.service";
                key = "{{HOMEPAGE_FILE_IMMICH_KEY}}";
              };
            };
          }
          {
            TubeArchivist = {
              href = "http://tubearchivist.service";
              icon = "tubearchivist";
              widget = {
                type = "tubearchivist";
                url = "http://tubearchivist.service";
                key = "{{HOMEPAGE_FILE_TUBEARCHIVIST_KEY}}";
              };
            };
          }
        ];
      }
      {
        "File Mangement" = [
          {
            qBittorrent = {
              href = "http://qbittorrent.service";
              icon = "qbittorrent";
              widget = {
                type = "qbittorrent";
                url = "http://qbittorrent.service";
                username = "solomon";
                password = "{{HOMEPAGE_FILE_QBITTORRENT_PASSWORD}}";
              };
            };
          }
          {
            lidarr = {
              href = "http://lidarr.service";
              icon = "lidarr";
              widget = {
                type = "lidarr";
                url = "http://lidarr.service";
                key = "{{HOMEPAGE_FILE_LIDARR_KEY}}";
              };
            };
          }
          {
            radarr = {
              href = "http://radarr.service";
              icon = "radarr";
              widget = {
                type = "radarr";
                url = "http://radarr.service";
                key = "{{HOMEPAGE_FILE_RADARR_KEY}}";
              };
            };
          }
          {
            sonarr = {
              href = "http://sonarr.service";
              icon = "sonarr";
              widget = {
                type = "sonarr";
                url = "http://sonarr.service";
                key = "{{HOMEPAGE_FILE_SONARR_KEY}}";
              };
            };
          }
          {
            prowlarr = {
              href = "http://prowlarr.service";
              icon = "prowlarr";
              widget = {
                type = "prowlarr";
                url = "http://prowlarr.service";
                key = "{{HOMEPAGE_FILE_PROWLARR_KEY}}";
              };
            };
          }
          {
            sabnzbd = {
              href = "http://sabnzbd.service";
              widget = {
                type = "sabnzbd";
                url = "http://sabnzbd.service";
                key = "{{HOMEPAGE_FILE_SABNZBD_KEY}}";
              };
            };
          }
        ];
      }
      {
        Infrastructure = [
          {
            Grafana = {
              href = "http://storm-bird/d/rYdddlPWk/node-exporter-full?orgId=1&refresh=1m";
              icon = "grafana";
              description = "System statistics and graphs";
              server = "docker2";
            };
          }
          {
            truenas = {
              href = "http://sandra-voi.machine";
              icon = "truenas";
              widget = {
                type = "truenas";
                url = "http://sandra-voi";
                key = "{{HOMEPAGE_FILE_TRUENAS_KEY}}";
              };
            };
          }
          {
            "Enphase Solar Monitor" = {
              href = "https://enlighten.enphaseenergy.com/web";
            };
          }
        ];
      }
    ];
    widgets = [
      {
        datetime = {
          text_size = "xl";
          format = {
            dateStyle = "long";
            timeStyle = "short";
            hour12 = false;
          };
        };
      }
      {
        search = {
          provider = "duckduckgo";
          target = "_blank";
        };
      }
      {
        openmeteo = {
          label = "Los Angeles";
          latitude = 34.25193046871679;
          longitude = -118.37664240064478;
          timezone = "America/Los_Angeles";
          units = "metric"; # or imperial
          cache = 5; # Time in minutes to cache API responses, to stay within limits
        };
      }
    ];
  };

  systemd.services.homepage-dashboard = {
    serviceConfig = {
      DynamicUser = pkgs.lib.mkOverride 10 false;
    };
  };

  environment.etc."homepage.env".text = ''
    # Organization
    HOMEPAGE_FILE_PAPERLESS_PASSWORD=${config.sops.secrets.paperless-password.path}
    HOMEPAGE_FILE_HOMEBOX_PASSWORD=${config.sops.secrets.homebox-password.path}
    HOMEPAGE_FILE_HOMEASSISTANT_KEY=${config.sops.secrets.home-assistant-access-token.path}

    # Media
    HOMEPAGE_FILE_JELLYSEER_KEY=${config.sops.secrets.jellyseer-key.path}
    HOMEPAGE_FILE_JELLYFIN_KEY=${config.sops.secrets.jellyfin-key.path}
    HOMEPAGE_FILE_NAVIDROME_TOKEN=${config.sops.secrets.navidrome-token.path}
    HOMEPAGE_FILE_NAVIDROME_SALT=${config.sops.secrets.navidrome-salt.path}
    HOMEPAGE_FILE_IMMICH_KEY=${config.sops.secrets.immich-key.path}
    HOMEPAGE_FILE_TUBEARCHIVIST_KEY=${config.sops.secrets.TubeArchivist-key.path}

    # File Management
    HOMEPAGE_FILE_QBITTORRENT_PASSWORD=${config.sops.secrets.qBittorrent-password.path}
    HOMEPAGE_FILE_LIDARR_KEY=${config.sops.secrets.lidarr-key.path}
    HOMEPAGE_FILE_RADARR_KEY=${config.sops.secrets.radarr-key.path}
    HOMEPAGE_FILE_SONARR_KEY=${config.sops.secrets.sonarr-key.path}
    HOMEPAGE_FILE_PROWLARR_KEY=${config.sops.secrets.prowlarr-key.path}
    HOMEPAGE_FILE_SABNZBD_KEY=${config.sops.secrets.sabnzbd-key.path}

    # Infrastructure
    HOMEPAGE_FILE_TRUENAS_KEY=${config.sops.secrets.truenas-key.path}
  '';

  sops.secrets = {
    paperless-password = {};
    homebox-password = {};
    home-assistant-access-token = {};
    
    jellyseer-key = {};
    jellyfin-key = {};
    navidrome-token = {};
    navidrome-salt = {};
    immich-key = {};
    TubeArchivist-key = {};
    
    qBittorrent-password = {};
    lidarr-key = {};
    radarr-key = {};
    sonarr-key = {};
    prowlarr-key = {};
    sabnzbd-key = {};
    truenas-key = {};
  };

  services.nginx.virtualHosts."homepage.service" = {
    locations."/".proxyPass = "http://localhost:3000";
  };
}

