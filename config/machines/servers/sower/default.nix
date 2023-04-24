{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    #./hoogle.nix
    ./immich.nix
    ./podgrab.nix
    ./navidrome.nix
    ./nfs.nix
    #./youtube-dl.nix

    ../../../profiles/physical-machine
    ../../../modules/services/docker
    ../../../modules/services/postgresql
    ../../../modules/services/jellyfin
  ];

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  environment.systemPackages = [
    pkgs.libva
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

  networking.firewall.allowedTCPPorts = [ 80 9002 ];

  services.prometheus.exporters = {
    node = {
      enable = true;
      enabledCollectors = [ "systemd" ];
      port = 9002;
    };
  };
}
