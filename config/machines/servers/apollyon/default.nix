# qBittorrent
# TODO: Disable tailscale
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./wireguard.nix

    ../../../profiles/virtual-machine
  ];

  networking.hostName = "apollyon";

  services.qBittorrent = {
    enable = true;
    openFirewall = true;
    webUIAddress.port = 8081;
  };

  fileSystems."/mnt/media" = {
    device = "192.168.5.6:/mnt/tank/Media";
    fsType = "nfs";
  };
}
