{ pkgs, config, ... }:

{
  sops.secrets.unpoller-unifi-password = {
    owner = "unifi-poller";
    mode = "0400";
  };

  sops.secrets.grafana-secret-key = {
    owner = "grafana";
    mode = "0400";
  };

  services.log-shipper.lokiUrl = "http://127.0.0.1:3101";

  services = {
    grafana = {
      enable = true;
      domain = "grafana.service.home.arpa";
      port = 2342;
      addr = "127.0.0.1";

      settings.security.secret_key = "$__file{${config.sops.secrets.grafana-secret-key.path}}";

      # Grafana sends its own alert mail. It reuses the friendly-ghost SMTP
      # credential rather than introducing a second one.
      settings.smtp = {
        enabled = true;
        host = "smtp.gmail.com:587";
        user = "ssbothwell@gmail.com";
        password = "$__file{${config.sops.secrets.friendly-ghost-smtp-password.path}}";
        from_address = "ssbothwell@gmail.com";
        from_name = "Grafana";
      };

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
          # Pinned to the value Grafana generated on its own. Changing it would
          # break every hand-built dashboard that already references it.
          uid = "P2A366ABB0CD47F51";
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

      provision.alerting = {
        contactPoints.settings = {
          apiVersion = 1;
          contactPoints = [{
            orgId = 1;
            name = "email";
            receivers = [{
              uid = "default-email";
              type = "email";
              settings.addresses = "ssbothwell@gmail.com";
            }];
          }];
          # Grafana upserts receivers by uid and keeps any receiver that leaves
          # this file. The former irrigation-email uid needs an explicit delete,
          # or the email contact point holds two receivers and sends twice.
          deleteContactPoints = [{
            orgId = 1;
            uid = "irrigation-email";
          }];
        };

        policies.settings = {
          apiVersion = 1;
          policies = [{
            orgId = 1;
            receiver = "email";
            group_by = [ "alertname" ];
            # A dead valve stays dead until someone walks out to it. The Grafana
            # default of 4 hours sent 94 mails for one flat battery.
            repeat_interval = "24h";
          }];
        };

        rules.settings = {
          apiVersion = 1;
          groups = [{
            orgId = 1;
            name = "irrigation";
            folder = "Irrigation";
            interval = "10m";
            rules = [
              {
                uid = "irrigation-valve-silent";
                title = "Irrigation valve silent";
                condition = "C";
                # Only valves that have reported before appear in this query, so
                # a valve that never exposed health entities cannot trigger it.
                "for" = "1h";
                noDataState = "OK";
                execErrState = "Error";
                labels = {
                  severity = "warning";
                  component = "irrigation";
                };
                annotations = {
                  summary = "Irrigation valve {{ $labels.device }} is silent.";
                  description = "{{ $labels.device }} last reported {{ printf \"%.0f\" $values.B.Value }} hours ago, past the 48 hour threshold.";
                };
                data = [
                  {
                    refId = "A";
                    datasourceUid = "P2A366ABB0CD47F51";
                    # This window does nothing here. Grafana only applies it to a
                    # SQL datasource through the $__timeFrom, $__timeTo, and
                    # $__timeFilter macros, and the query below uses none of them.
                    # Keep it that way. Home Assistant writes a row only when a
                    # value changes, so a healthy valve can hold a reading for
                    # weeks. A time filter would drop a long dead valve out of the
                    # result set and resolve the alert while the valve stayed dead.
                    relativeTimeRange = { from = 2592000; to = 0; };
                    model = {
                      refId = "A";
                      format = "table";
                      rawQuery = true;
                      rawSql = ''
                        SELECT regexp_replace(m.entity_id, '^sensor\.(.*)_linkquality$', '\1') AS device,
                               ((extract(epoch FROM now()) - max(s.last_updated_ts)) / 3600.0)::float8 AS hours_silent
                        FROM states s
                        JOIN states_meta m ON m.metadata_id = s.metadata_id
                        WHERE m.entity_id ~ '_linkquality$'
                          AND s.state NOT IN ('unknown','unavailable')
                        GROUP BY 1
                      '';
                    };
                  }
                  {
                    refId = "B";
                    datasourceUid = "__expr__";
                    model = {
                      refId = "B";
                      type = "reduce";
                      expression = "A";
                      reducer = "last";
                    };
                  }
                  {
                    refId = "C";
                    datasourceUid = "__expr__";
                    model = {
                      refId = "C";
                      type = "threshold";
                      expression = "B";
                      conditions = [{
                        evaluator = { type = "gt"; params = [ 48.0 ]; };
                      }];
                    };
                  }
                ];
              }
              {
                uid = "irrigation-valve-battery-low";
                title = "Irrigation valve battery low";
                condition = "C";
                # A flat battery is what precedes silence. manifold_2 read 0
                # percent for nine days before it dropped off the mesh, and the
                # silent valve rule only spoke up 48 hours after it was gone.
                "for" = "1h";
                noDataState = "OK";
                execErrState = "Error";
                labels = {
                  severity = "warning";
                  component = "irrigation";
                };
                annotations = {
                  summary = "Irrigation valve {{ $labels.device }} has a low battery.";
                  description = "{{ $labels.device }} reports {{ printf \"%.0f\" $values.B.Value }} percent battery. Replace it before the valve goes silent.";
                };
                data = [
                  {
                    refId = "A";
                    datasourceUid = "P2A366ABB0CD47F51";
                    # Inert for raw SQL. See the note on the silent valve rule.
                    relativeTimeRange = { from = 2592000; to = 0; };
                    model = {
                      refId = "A";
                      format = "table";
                      rawQuery = true;
                      # current_device_status is a SONOFF SWV entity. It selects
                      # the irrigation valves and leaves out every other battery
                      # device on the Zigbee mesh, such as the Hue dimmer.
                      rawSql = ''
                        WITH latest AS (
                          SELECT DISTINCT ON (m.entity_id)
                                 regexp_replace(m.entity_id, '^sensor\.(.*)_battery$', '\1') AS device,
                                 s.state::float8 AS battery_percent
                          FROM states s
                          JOIN states_meta m ON m.metadata_id = s.metadata_id
                          WHERE m.entity_id ~ '^sensor\..*_battery$'
                            AND s.state ~ '^[0-9]+(\.[0-9]+)?$'
                          ORDER BY m.entity_id, s.last_updated_ts DESC
                        )
                        SELECT device, battery_percent
                        FROM latest
                        WHERE EXISTS (
                          SELECT 1 FROM states_meta v
                          WHERE v.entity_id = 'sensor.' || latest.device || '_current_device_status'
                        )
                      '';
                    };
                  }
                  {
                    refId = "B";
                    datasourceUid = "__expr__";
                    model = {
                      refId = "B";
                      type = "reduce";
                      expression = "A";
                      reducer = "last";
                    };
                  }
                  {
                    refId = "C";
                    datasourceUid = "__expr__";
                    model = {
                      refId = "C";
                      type = "threshold";
                      expression = "B";
                      conditions = [{
                        evaluator = { type = "lt"; params = [ 15.0 ]; };
                      }];
                    };
                  }
                ];
              }
            ];
          }];
        };
      };
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
