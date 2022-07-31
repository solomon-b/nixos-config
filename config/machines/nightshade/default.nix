{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./kmonad.nix
    ./nfs.nix

    ../../profiles/pc
    ../../modules/services/virtualisation/libvirt
    ../../modules/services/syncthing
    ../../modules/services/tailscale
    ../../modules/system/devices/touchpad
    ../../modules/system/powertop
  ];

  nixpkgs.config.allowBroken = true;
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  environment.systemPackages = [
    pkgs.acpi
    pkgs.freecad
  ];

  primary-user.name = "solomon";

  networking = {
    hostName = "nightshade";
    hostId = "997f3c8d";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
  };
}
