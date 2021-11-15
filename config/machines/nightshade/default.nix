{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./wireguard.nix
    # ./kmonad.nix
    ./hoogle.nix
    ../../profiles/pc

    ../../modules/system/devices/touchpad
    ../../modules/system/powertop

    # TODO: Setup Wifi Networks???
    # ../../modules/system/devices/wifi
  ];

  nixpkgs.config.allowBroken = true;
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  environment.systemPackages = [
    pkgs.acpi
  ];

  primary-user.name = "solomon";

  networking = {
    hostName = "nightshade";
    hostId = "997f3c8d";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
    hosts = {
      "192.168.0.3" = [ "sower" ];
    };
  };
}
