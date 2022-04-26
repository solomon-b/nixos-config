{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    #./cardano-node.nix
    ./wireguard.nix
    ./kmonad.nix

    ../../profiles/physical-machine
    ../../modules/services/bittorrent
    #../../modules/services/virtualisation/libvirt
    ../../modules/services/plex
    ../../modules/services/jellyfin
  ];

  nixpkgs.config.allowBroken = true;
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  #environment.systemPackages = [
  #  pkgs.cardano-node
  #  pkgs.cardano-cli
  #  pkgs.cardano-wallet
  #  pkgs.tx-generator
  #];

  primary-user.name = "solomon";

  networking = {
    hostName = "sower";
    hostId = "960855f8";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
    hosts = {
      "192.168.0.3" = [ "sower" ];
    };
  };
}
