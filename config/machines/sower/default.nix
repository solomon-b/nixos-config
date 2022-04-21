{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    #./cardano-node.nix
    #./wireguard.nix
    ./kmonad.nix
    ../../modules/system/bittorrent
    ../../modules/system/virtualisation/libvirt
    ../../profiles/pc
  ];

  nixpkgs.config.allowBroken = true;
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  environment.systemPackages = [
    pkgs.acpi
    #pkgs.freecad
    #pkgs.cardano-node
    #pkgs.cardano-cli
    #pkgs.cardano-wallet
    #pkgs.tx-generator
  ];

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
