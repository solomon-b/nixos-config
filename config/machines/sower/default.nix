{ pkgs, ... }:

{
  imports = [
    ./hardware.nix

    #./cardano-node.nix
    ./wireguard.nix
    ./kmonad.nix
    ./nfs.nix

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
    interfaces.eno1.useDHCP = true;
    useDHCP = false;
  };
}
