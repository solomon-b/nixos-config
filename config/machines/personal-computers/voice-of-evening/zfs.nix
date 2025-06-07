# Sending and Receiving ZFS Data:
# https://docs.oracle.com/cd/E18752_01/html/819-5461/gbchx.html
#
# Required Target Permission:
#   compression,create,destroy,hold,mount,mountpoint,receive,refreservation,release,rollback,send,snapshot
#   https://github.com/jimsalterjrs/sanoid/issues/660
{ config, options, ... }:

{
  services.zfs = {
    autoScrub.enable = true;
    autoSnapshot = {
      enable = true;
      frequent = 4;
      hourly = 24;
      weekly = 4;
      monthly = 3;
    };
    trim.enable = true;
  };

  services.syncoid = {
    enable = false;
    sshKey = config.sops.secrets.syncoid-ssh-key.path;
    commands = {
      "tank/home" = {
        target = "syncoid@sandra-voi.home.arpa:tank/system-snapshots/voice-of-evening/home";
        recursive = true;
      };
      "tank/root" = {
        target = "syncoid@sandra-voi.home.arpa:tank/system-snapshots/voice-of-evening/root";
        recursive = true;
      };
    };
  };

  # "Exactly one of users.users.syncoid.isSystemUser and users.users.syncoid.isNormalUser must be set."
  # WTF these should be defined already:
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/services/backup/syncoid.nix#L343
  users.users.syncoid.group = "syncoid";
  users.users.syncoid.isSystemUser = true;
  users.groups.syncoid = {};

  users.users.syncoid.extraGroups = [ "keys" ];
}
