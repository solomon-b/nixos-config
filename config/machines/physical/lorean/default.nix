{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./kmonad.nix

    ../../../profiles/pc
    ../../../modules/services/virtualisation/libvirt
    ../../../modules/system/devices/touchpad
    ../../../modules/system/powertop
  ];

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  environment.systemPackages = [
    pkgs.acpi
  ];

  primary-user.name = "solomon";

  networking = {
    hostName = "lorean";
    hostId = "960855f8";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp4s0.useDHCP = true;
  };
}
