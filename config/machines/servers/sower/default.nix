{ config, pkgs, ... }:

{
  imports = [
    ./hardware.nix
    #./hoogle.nix
    ./home-assistant.nix
    ./immich.nix
    ./jellyfin.nix
    #./podgrab.nix
    ./soulseek.nix
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

  services.friendly-ghost = {
    enable = true;
    journal = {
      units = [ "nginx" "immich-server" "immich-machine-learning" "jellyfin" "navidrome" "docker-home-assistant" ];
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

  primary-user.extraGroups = [ "docker" ];
}
