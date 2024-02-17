{ config, ... }:

let
  homeDir = config.primary-user.home-manager.home.homeDirectory;
in
{
  primary-user.home-manager.home = rec {
    sessionPath = [
      "${homeDir}/.cabal/bin"
      "${sessionVariables.XDG_BIN_HOME}"
    ];

    sessionVariables = {
      VISUAL = "vim";
      EDITOR = "vim";

      XDG_CACHE_HOME = "${homeDir}/.cache";
      XDG_CONFIG_HOME = "${homeDir}/.config";
      XDG_DATA_HOME = "${homeDir}/.local/share";
      XDG_STATE_HOME = "${homeDir}/.local/state";

      # Not officially in the specification
      XDG_BIN_HOME = "${homeDir}/.local/bin";
    };
  };
}
