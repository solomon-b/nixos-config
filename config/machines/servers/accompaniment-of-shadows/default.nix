{ pkgs, config, inputs, ... }:

{
  imports = [
    ./hardware.nix

    ./coredns.nix
    ./forgejo.nix
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
    firewall.allowedTCPPorts = [ 53 80 3101 3300 8080 9090 ];
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
      # Docker copies the host resolv.conf into each container at creation time.
      # During boot, resolvconf rewrites that file, so containers created in the
      # wrong moment get no nameserver and lose all DNS. Pin the resolvers here:
      # coredns first for home.arpa names, then a public fallback.
      daemon.settings.dns = [ "192.168.5.105" "1.1.1.1" ];
    };
    oci-containers.backend = "docker";
  };

  primary-user.extraGroups = [ "docker" ];

  services.micasa = {
    enable = true;
    package = inputs.micasa.packages.${pkgs.system}.default;
    authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
  };
}
