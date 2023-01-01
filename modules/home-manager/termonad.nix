{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.programs.termonad;
in
{
  options.programs.termonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable termonad";
    };

    config = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Filepath for termonad config file.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.termonad ];
    xdg.configFile."termonad/termonad.hs".source = cfg.config;
  };
}
