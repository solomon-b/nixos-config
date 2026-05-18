{ pkgs, config, ... }:

{
  sops.secrets.unpoller-unifi-password = {
    owner = "unifi-poller";
    mode = "0400";
  };

  services.log-shipper = {
    enable = true;
    lokiUrl = "http://127.0.0.1:3101";
  };

  services = {
    grafana = {
      enable = true;
      domain = "grafana.service.home.arpa";
      port = 2342;
      addr = "127.0.0.1";

      provision.datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          url = "http://localhost:9090";
          isDefault = true;
        }
        {
          name = "Loki";
          type = "loki";
          url = "http://localhost:3101";
        }
        {
          name = "Home Assistant";
          type = "postgres";
          url = "localhost:5432";
          user = "hass";
          jsonData = {
            database = "hass";
            sslmode = "disable";
            postgresVersion = 1500;
          };
          secureJsonData.password = "hass";
        }
      ];
    };

    prometheus = {
      enable = true;

      scrapeConfigs = [
        {
          job_name = "node";
          static_configs = [{
            targets = builtins.map (s: "${s}:9002") (builtins.attrNames (builtins.readDir ../.));
          }];
        }
        {
          job_name = "unpoller";
          static_configs = [{
            targets = [ "127.0.0.1:9130" ];
          }];
        }
      ];
    };

    unpoller = {
      enable = true;
      poller.quiet = true;
      influxdb.disable = true;
      prometheus.http_listen = "127.0.0.1:9130";
      loki = {
        url = "http://127.0.0.1:3101";
        interval = "2m";
      };
      unifi.controllers = [{
        url = "https://192.168.1.1";
        user = "unifi-poller";
        pass = config.sops.secrets.unpoller-unifi-password.path;
        sites = "all";
        save_ids = false;
        save_events = false;
        save_alarms = true;
        save_anomalies = true;
        save_sites = true;
        verify_ssl = false;
      }];
    };

    loki = {
      enable = true;
      configuration = {
        auth_enabled = false;
        server = {
          http_listen_port = 3101;
          grpc_listen_port = 9096;
        };
        common = {
          instance_addr = "127.0.0.1";
          path_prefix = "/var/lib/loki";
          storage.filesystem = {
            chunks_directory = "/var/lib/loki/chunks";
            rules_directory = "/var/lib/loki/rules";
          };
          replication_factor = 1;
          ring.kvstore.store = "inmemory";
        };
        schema_config.configs = [{
          from = "2024-01-01";
          store = "tsdb";
          object_store = "filesystem";
          schema = "v13";
          index = {
            prefix = "index_";
            period = "24h";
          };
        }];
        limits_config = {
          retention_period = "720h";
          reject_old_samples = true;
          reject_old_samples_max_age = "168h";
        };
        compactor = {
          working_directory = "/var/lib/loki/compactor";
          retention_enabled = true;
          retention_delete_delay = "2h";
          delete_request_store = "filesystem";
        };
      };
    };

    uptime-kuma = {
      enable = true;
    };

    nginx.virtualHosts = {
      "grafana.service.home.arpa" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:2342";
          proxyWebsockets = true;
        };
      };

      "uptime.service.home.arpa" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:3001";
          proxyWebsockets = true;
        };
      };
    };
  };
}
