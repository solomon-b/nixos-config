{ config, ... }:

let
  homeDir = config.primary-user.home-manager.home.homeDirectory;
in
{
  primary-user.home-manager.home = {
    sessionPath = [
      "${homeDir}/.local/scripts"
      "${homeDir}/.local/bin"
      "${homeDir}/.cabal/bin"
    ];

    sessionVariables = {
      VISUAL = "vim";
      EDITOR = "vim";
      SCRIPTS = "${homeDir}/.local/scripts";
    };
  };
}
