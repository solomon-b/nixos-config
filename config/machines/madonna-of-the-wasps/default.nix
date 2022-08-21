{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./nfs.nix

    ../../profiles/physical-machine
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
    hostName = "madonna-of-the-wasps";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };

  services.nextcloud = {                
    enable = true;                   
    package = pkgs.nextcloud24;
    hostName = "localhost";
    config.adminpassFile = "/secrets/nextcloud-admin-pass";
    datadir = "/mnt/Nextcloud_Data";
  };
}
