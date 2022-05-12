{ pkgs, ... }:

{
  imports = [
    ./hardware.nix

    ./wireguard.nix
    ./nginx.nix

    ../../profiles/physical-machine
    ../../modules/services/bittorrent
    ../../modules/services/dns
    ../../modules/services/jellyfin
    ../../modules/services/syncthing
  ];

  nixpkgs.config.allowBroken = true;
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  primary-user.name = "solomon";

  networking = {
    hostName = "sower";
    hostId = "960855f8";
    interfaces.eno1.useDHCP = true;
    useDHCP = false;

    hosts = {
      "192.168.0.1" = [ "router" ];
    };
  };
}
