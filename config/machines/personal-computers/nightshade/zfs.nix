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
    autoSnapshot.enable = true;
    trim.enable = true;
  };

  services.syncoid = {
    enable = true;
    sshKey = /secrets/syncoid-ssh-key;
    commands = {
      "tank/home" = {
        target = "syncoid@sandra-voi.local:tank/system-snapshots/nightshade/home";
        recursive = true;
      };
      "tank/root" = {
        target = "syncoid@sandra-voi.local:tank/system-snapshots/nightshade/root";
        recursive = true;
      };
    };
  };
}
