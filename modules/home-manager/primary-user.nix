############################################################################
# OS-agnostic home config options for the system's primary account holder. #
############################################################################
{
  config,
  lib,
  options,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkAliasDefinitions
    mkAliasOptionModule
    mkIf
    mkOption
    types
    ;
  inherit (pkgs.buildPlatform) isDarwin;
  cfg = config.primary-user;
in
{
  options.primary-user.name = mkOption {
    type = types.nullOr types.str;
    default = "solomon";
    description = "The primary account holder's username (defaults to 'solomon').";
  };

  options.primary-user.home = mkOption {
    type = types.nullOr types.str;
    default = if isDarwin then "/Users/${cfg.name}" else "/home/${cfg.name}";
    description = "The primary account holder's home directory.";
  };

  config = mkIf (cfg.name != null) {
    home.username = cfg.name;
    home.homeDirectory = cfg.home;
    home.stateVersion = lib.mkDefault "23.05";

    # By default, allow `home-manager` to manage its own installation.
    programs.home-manager.enable = lib.mkDefault true;
  };
}
