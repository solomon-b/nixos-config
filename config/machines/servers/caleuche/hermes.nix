{ config, pkgs, inputs, ... }:

let
  cfg = config.services.hermes-agent;

  # Runs the CLI as the hermes user, so the agent never runs as solomon.
  # -i starts in the hermes home, which is the state directory.
  hermes-cli = pkgs.writeShellScriptBin "hermes" ''
    exec sudo -u ${cfg.user} -i -- ${cfg.package}/bin/hermes "$@"
  '';
in
{
  imports = [ inputs.hermes-agent.nixosModules.default ];

  sops.secrets = {
    openrouter-api-key = { };
    dashboard-password = { };
    dashboard-session-secret = { };
  };

  sops.templates."hermes-env".content = ''
    OPENROUTER_API_KEY=${config.sops.placeholder.openrouter-api-key}
    HERMES_DASHBOARD_BASIC_AUTH_PASSWORD=${config.sops.placeholder.dashboard-password}
    HERMES_DASHBOARD_BASIC_AUTH_SECRET=${config.sops.placeholder.dashboard-session-secret}
  '';

  services.hermes-agent = {
    enable = true;
    package = inputs.hermes-agent.packages.${pkgs.stdenv.hostPlatform.system}.minimal;

    environmentFiles = [ config.sops.templates."hermes-env".path ];

    # The file tools refuse writes outside the state directory.
    environment.HERMES_WRITE_SAFE_ROOT = cfg.stateDir;

    settings = {
      model = {
        provider = "openrouter";
        default = "deepseek/deepseek-v4.1-flash";
      };

      platform_toolsets.cli = [ "terminal" "file" "todo" "clarify" ];

      # Ask Solomon before a flagged command. "smart" asks another model.
      approvals.mode = "manual";

      # The browser workspace, behind nginx. A public_url turns on sign-in.
      webapp = {
        enabled = true;
        host = "127.0.0.1";
        port = 9120;
        public_url = "http://hermes.service.home.arpa";
      };

      dashboard.basic_auth.username = "solomon";
    };

    # grafana-loki provides logcli.
    extraPackages = with pkgs; [ curl jq grafana-loki ];

    hermesHomeFiles."SOUL.md" = ''
      You help Solomon find problems in the homelab. You only read. Do not
      change, restart, or delete anything on any host.

      Metrics: Prometheus at http://accompaniment-of-shadows:9090, no auth.
      Use the HTTP API, for example:
        curl -s 'http://accompaniment-of-shadows:9090/api/v1/query' \
          --data-urlencode 'query=node_systemd_unit_state{state="failed"} == 1'
      Each server is the instance "<hostname>:9002". Check the "up" metric
      first. A target that is down has no unit data.

      Logs: Loki at http://accompaniment-of-shadows:3101, no auth. All
      servers send logs. Labels: host,
      service_name (the systemd unit, for example "nginx.service"),
      container_name (for Docker containers, for example "tubearchivist"),
      and level (can be "unknown"). Use logcli, for example:
        logcli --addr=http://accompaniment-of-shadows:3101 query \
          --since=1h '{host="sower", service_name="jellyfin.service"}'
        logcli --addr=http://accompaniment-of-shadows:3101 query \
          --since=1h '{host="sower", container_name="tubearchivist"}'

      Do not push to Loki. Do not reach other hosts or ports.
    '';
  };

  services.nginx.virtualHosts."hermes.service.home.arpa" = {
    extraConfig = "proxy_buffering off;";
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString cfg.settings.webapp.port}";
      proxyWebsockets = true;
    };
  };

  # The module turns on linger for cron jobs. We use no cron jobs, and
  # linger lets the agent leave timers that run after the CLI exits.
  users.users.hermes.linger = false;

  users.users.solomon.packages = [ hermes-cli ];
}
