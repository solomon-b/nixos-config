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
        target = "syncoid@sandra-voi:tank/system-snapshots/voice-of-evening/home";
        recursive = true;
      };
      "tank/root" = {
        target = "syncoid@sandra-voi:tank/system-snapshots/voice-of-evening/root";
        recursive = true;
      };
    };

    localSourceAllow = options.services.syncoid.localSourceAllow.default ++ [
      "mount"
    ];

    localTargetAllow = options.services.syncoid.localTargetAllow.default ++ [
      "destroy"
    ];
  };

  # TODO: This is defined in the Nixos module but for some reason I'm getting an
  # error about it being unset.
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/services/backup/syncoid.nix#L117
  users.users.syncoid.group = "syncoid";
  # TODO: Same issue here.
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/services/backup/syncoid.nix#L301
  users.users.syncoid.isSystemUser = true;

  # Ditto for the group:
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/services/backup/syncoid.nix#L309
  users.groups.syncoid = { };

  # Allow syncoid user to access SOPS secrets.
  users.users.syncoid.extraGroups = [ "keys" ];
}
