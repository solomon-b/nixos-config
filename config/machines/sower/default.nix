{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./freshrss.nix
    ./heimdall.nix
    ./podcast-dl.nix
    #./nextcloud.nix
    ./nginx.nix
    ./wireguard.nix

    ../../profiles/physical-machine
    ../../modules/services/bittorrent
    #../../modules/services/dns
    ../../modules/services/docker
    ../../modules/services/postgresql
    ../../modules/services/tailscale
    ../../modules/services/jellyfin
    ../../modules/services/syncthing
  ];

  nixpkgs.config.allowBroken = true;
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
}
