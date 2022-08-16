{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./nfs.nix
    ./wireguard.nix

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
    hostName = "apollyon";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };

  services.qBittorrent = {
    enable = true;
    openFirewall = true;
    webUIAddress.port = 8081;
  };
}
