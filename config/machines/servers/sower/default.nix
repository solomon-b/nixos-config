{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    #./hoogle.nix
    #./podcast-dl.nix
    #./photoprism.nix
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

  # 8082 == FreshRSS
  # 8083 == Hoogle
  # 8096 == Jellyfin
  # 2342 == Photoprism

  networking.firewall.allowedTCPPorts = [ 80 ];
}
