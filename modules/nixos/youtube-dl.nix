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
        A list of xml feeds for youtubes to archive.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.run-youtube-dl = {
      serviceConfig.Type = "oneshot";
      script = pkgs.writeShellScriptBin "run"
        ''
        set -e
        cd ${cfg.dataDir}

        ${pkgs.yt-dlp}/bin/yt-dlp \
          -i \
          -o '%(uploader)s (%(uploader_id)s)/%(upload_date)s - %(title)s - [%(resolution)s] [%(id)s].%(ext)s' \
          --merge-output-format mkv \
          --write-subs \
          --sub-langs all \
          --convert-subs srt \
          --add-metadata \
          --write-description \
          --write-thumbnail \
          "${lib.concatStringsSep " " cfg.youtubes}"
        '';
    };

    systemd.timers.run-youtube-dl = {
      wantedBy = [ "timers.target" ];
      partOf = [ "run-youtube-dl.service" ];
      timerConfig.OnCalendar = "Sat, 04:00:00";
    };
  };
}
