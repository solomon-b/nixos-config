{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./kmonad.nix
    ./nfs.nix
    ./zfs.nix

    ../../../profiles/pc
    ../../../modules/services/virtualisation/virtualbox
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

  sops.secrets.syncoid-ssh-key = {
    owner = "syncoid";
    mode = "600";
  };

  networking = {
    hostName = "zodiacal-light";
    hostId = "02e2867c";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };
}
