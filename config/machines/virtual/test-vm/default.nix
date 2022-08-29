{ config, lib, pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
  ];

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  primary-user.name = "solomon";

  networking = {
    hostName = "test-vm";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };

  fileSystems."/mnt/media" = {
    device = "192.168.1.174:/mnt/tank/Media";
    fsType = "nfs";
  };

  system.stateVersion = "22.05";
}
