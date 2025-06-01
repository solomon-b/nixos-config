{ config, pkgs, ... }:

{
  imports = [
    ./hardware.nix
    #./hoogle.nix
    ./home-assistant.nix
    ./immich.nix
    ./jellyfin.nix
    #./podgrab.nix
    ./navidrome.nix
    ./tubearchivist.nix

    ../../../profiles/physical-machine
  ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  environment.systemPackages = [
    pkgs.libva
    pkgs.postgresql
  ];

  primary-user.name = "solomon";

  networking = {
    hostName = "sower";
    hostId = "960855f8";
    interfaces.eno1.useDHCP = true;
    useDHCP = false;
  };

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
  };

  # 8081 == podgrab
  # 8082 == FreshRSS
  # 8083 == Hoogle
  # 8096 == Jellyfin

  networking.firewall.allowedTCPPorts = [ 80 9002 3000];

  services.prometheus.exporters = {
    node = {
      enable = true;
      enabledCollectors = [ "systemd" ];
      port = 9002;
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

  services.irrigation-web-server = {
    enable = true;

    environment = "Production";
    hostname = "localhost";

    postgres = {
      db = "irrigation";
      host = "transfigured-night";
      passwordFile = config.sops.secrets.immich-postgres-password.path;
      port = 5432;
      user = "irrigation";
    };

    warp = {
      port = 3000;
      serverName = "localhost";
      timeout = "100";
    };

    observability = {
      exporter = "StdOut";
    };

    otel = {
      sampler = "always_on";
      serviceName = "irriation-web-server";
      endpoint = "http://localhost:4318";
      protocol = "http/protobuf";
    };
  };

  sops.secrets = {
    irrigation-postgres-password = { };
  };
}
