# Prometheus/Grafana Monitoring
{ pkgs, config, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
  ];

  services.prometheus = {
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

  services.grafana = {
    enable = true;
    domain = "storm-bird";
    port = 2342;
    addr = "127.0.0.1";
  };

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;

    virtualHosts.${config.services.grafana.domain} = {
      locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
          proxyWebsockets = true;
      };
    };
  };

  networking.hostName = "storm-bird";

  networking.firewall.allowedTCPPorts = [ 80 ];
}
