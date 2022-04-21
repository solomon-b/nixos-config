{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.qbittorrent-nox ];

  networking.firewall.allowedTCPPorts = [ 8080 ];
}
