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
    firewall.allowedTCPPorts = [ 53 80 8080 ];
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
    };
    oci-containers.backend = "docker";
  };

  primary-user.extraGroups = [ "docker" ];

  services.friendly-ghost = {
    enable = true;
    journal = {
      units = [ "nginx" "postgresql" "redis" "coredns" ];
      priority = "warning";
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
