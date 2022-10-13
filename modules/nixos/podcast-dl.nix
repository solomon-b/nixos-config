{ config, lib, pkgs, ... }:

let
  cfg = config.services.podcast-dl;
  script = pkgs.writeShellScriptBin "run"
    ''
    set -e
    cd ${cfg.dataDir}

    IFS=' ' read -ra feeds <<< "${lib.concatStringsSep " " cfg.podcasts}"

    for feed in "''${feeds[@]}"
    do
      ${pkgs.podcast-dl}/bin/podcast-dl --url "''${feed}" &
    done
    '';
in
{
  options.services.podcast-dl = with lib; {
    enable = mkEnableOption "Podcast-dl, a self-hosted podcast manager";

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/${defaultUser}/data";
      example = "/home/yourUser/data";
      description = ''
        The path where podcast-dl will save podcast data.
      '';
    };

    podcasts = mkOption {
      type = types.listOf types.str;
      default = [];
      example = [ "https://feed.site.com/feed.xml" ];
      description = ''
        A list of xml feeds for podcasts to archive.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.run-podcast-dl = {
      serviceConfig.Type = "oneshot";
      script = ''
        echo "Excecuting podcast-dl
        ${pkgs.bash}/bin/bash ${script}/bin/run
        echo "Completed podcast-dl  
      '';
    };

    systemd.timers.run-podcast-dl = {
      wantedBy = [ "timers.target" ];
      partOf = [ "run-podcast-dl.service" ];
      timerConfig.OnCalendar = "Sun, 04:00:00";
    };
  };
}
