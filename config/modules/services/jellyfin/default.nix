{ ... }:

{
  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  users.groups.nas.gid = 998;
  users.users.jellyfin.extraGroups = [ "nas" ];
}
