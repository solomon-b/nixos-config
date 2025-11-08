{ pkgs, ... }:

{
  services = {
    grafana = {
      enable = true;
      domain = "grafana.service.home.arpa";
      port = 2342;
      addr = "127.0.0.1";
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
      ];
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
