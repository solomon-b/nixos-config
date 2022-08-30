# qBittorrent
# TODO: Disable tailscale
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./wireguard.nix

    ../../../profiles/virtual-machine
  ];

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

  fileSystems."/mnt/media" = {
    device = "192.168.1.174:/mnt/tank/Media";
    fsType = "nfs";
  };
}
