{ config, lib, pkgs, ... }:

let
  cfg = config.services.youtube-dl;
in
{
  options.services.youtube-dl = with lib; {
    enable = mkEnableOption "Youtube-dl, a self-hosted podcast manager";

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/${defaultUser}/data";
      example = "/home/yourUser/data";
      description = ''
        The path where youtube-dl will save channel data.
      '';
    };

    youtubes = mkOption {
      type = types.listOf types.str;
      default = [];
      example = [ "https://www.youtube.com/channel/UCu6mSoMNzHQiBIOCkHUa2Aw" ];
      description = ''
        A list of xml feeds for yt-dlp to archive.
      '';
    };

    onCalendar = mkOption {
      type = types.str;
      default = "Sat, 04:00:00";
      example = "Sat, 04:00:00";
      description = ''
        The schedule to run youtube-dl in cron onCalendar format.
      '';
    };

    configFlags = mkOption {
      type = types.listOf types.str;
      default = null;
      description = ''
        An optional list of configuration flags.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.run-youtube-dl = {
      serviceConfig.Type = "oneshot";
      script =
        ''
        set -e
        cd ${cfg.dataDir}
        
        ${pkgs.yt-dlp}/bin/yt-dlp ${lib.concatStringsSep " " cfg.configFlags} "${lib.concatStringsSep " " cfg.youtubes}"
        '';
    };

    systemd.timers.run-youtube-dl = {
      wantedBy = [ "timers.target" ];
      partOf = [ "run-youtube-dl.service" ];
      timerConfig.OnCalendar = "Sat, 04:00:00";
    };
  };
}
