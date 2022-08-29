{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./nfs.nix
    ./wireguard.nix

    ../../../profiles/virtual-machine
  ];

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

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
