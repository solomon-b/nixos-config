{ pkgs, ... }:

{
  services.qBittorrent.enable = true;

  users.users.qBittorrent.extraGroups = [ "nas" ];
}
