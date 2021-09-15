{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.programs.xmobar;
in
{
  options.programs.xmobar = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable xmobar";
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ (import ../../overlays/xmobar-solomon.nix) ];
    home.packages = [ pkgs.xmobar ];
  };
}
