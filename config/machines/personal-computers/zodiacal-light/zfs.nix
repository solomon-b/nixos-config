# Sending and Receiving ZFS Data:
# https://docs.oracle.com/cd/E18752_01/html/819-5461/gbchx.html
#
# Required Target Permission:
#   compression,create,destroy,hold,mount,mountpoint,receive,refreservation,release,rollback,send,snapshot
#   https://github.com/jimsalterjrs/sanoid/issues/660
{ config, ... }:

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
    enable = true;
    sshKey = config.sops.secrets.syncoid-ssh-key.path;
    commands = {
      "tank/user" = {
        target = "syncoid@sandra-voi:tank/system-snapshots/zodiacal-light/user";
        recursive = true;
      };
      "tank/root" = {
        target = "syncoid@sandra-voi:tank/system-snapshots/zodiacal-light/root";
        recursive = true;
      };
    };
  };

  users.users.syncoid.extraGroups = [ "keys" ];
}
