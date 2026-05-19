{ pkgs, config, inputs, ... }:

{
  imports = [
    ./hardware.nix

    ./coredns.nix
    ./filebrowser.nix
    ./homebox.nix
    ./homepage.nix
    ./lubelogger.nix
    ./observability.nix
    ./paperless-ngx.nix
    ./planka.nix
    ./postgresql.nix
    ./redis.nix
    ./sabnzbd.nix
    ./servarr.nix
    ./tailscale.nix

    ../../../profiles/virtual-machine
  ];

  networking = {
    hostName = "accompaniment-of-shadows";
    firewall.allowedTCPPorts = [ 53 80 3101 8080 ];
    firewall.allowedUDPPorts = [ 53 ];
  };

  services = {
    nginx = {
      enable = true;

      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
    };

    nginx.virtualHosts = {
      "qbittorrent.service.home.arpa" = {
        locations."/" = {
          proxyPass = "http://192.168.5.104:8081";
        };
      };
    };
  };

  virtualisation = {
    containers = {
      enable = true;
    };

    docker = {
      enable = true;
      storageDriver = "overlay2";
      daemon.settings.log-driver = "journald";
    };
    oci-containers.backend = "docker";
  };

  primary-user.extraGroups = [ "docker" ];

  services.friendly-ghost = {
    enable = true;
    source = "loki";

    loki = {
      url = "http://127.0.0.1:3101";
      auth = "none";
      query = ''{service_name!=""}'';
    };

    filter = {
      units = [ ''.*\.service'' ];
      priority = "warning";
      ignorePatterns = [
        # alloy and loki are the monitoring substrate itself; their own warnings
        # about pushing/querying logs would create a feedback loop.
        "alloy"
        "loki"
        # HA bluetooth subsystem runs a permission check every restart even
        # though no integration uses it; the warning is benign.
        "Missing required permissions for Bluetooth"
        # HA emits this for every custom integration on every startup.
        "custom integration .* which has not been tested by Home Assistant"
        # irrigation_unlimited compares switches against Zigbee devices that
        # report "unknown" briefly during HA boot before re-reporting state.
        "irrigation_unlimited.*SYNCHRONISATION.*found: unknown"
        # Some container's locale init writes to stderr, which Docker's
        # journald driver mislabels as priority=err regardless of content.
        "^Language set to .+$"
      ];
    };

    email = {
      smtpHost = "smtp.gmail.com";
      smtpPort = 587;
      username = "ssbothwell@gmail.com";
      from = "ssbothwell@gmail.com";
      to = [ "ssbothwell@gmail.com" ];
      passwordFile = config.sops.secrets.friendly-ghost-smtp-password.path;
    };
  };

  sops.secrets.friendly-ghost-smtp-password = {
    mode = "0444";
  };

  services.micasa = {
    enable = true;
    package = inputs.micasa.packages.${pkgs.system}.default;
    authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
  };
}
