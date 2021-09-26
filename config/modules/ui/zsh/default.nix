{ pkgs, lib, config, ... }:

{
  primary-user.home-manager.programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableHistorySubstringSearch = true;
    autoSuggestions.highlightStyle = "fg=3";

    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
      save = 10000;
      share = true;
      size = 50000;
    };

    saveNoDups = true; # This should be in the history submodule but I dont own it.

    historySubstring = {
      highlightFound = "fg=green";
      highlightNotFound = "fg=red";
    };

    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.1.0";
          sha256 = "0snhch9hfy83d4amkyxx33izvkhbwmindy0zjjk28hih1a9l2jmx";
        };
      }
    ];

    initExtra = ''
      bindkey '^l' autosuggest-accept
      bindkey '^h' autosuggest-clear
      bindkey '^k' history-substring-search-up
      bindkey '^j' history-substring-search-down

      # Load Functions
      if [ -f ${config.primary-user.home-manager.home.sessionVariables.SCRIPTS}/functions ]; then
          source ${config.primary-user.home-manager.home.sessionVariables.SCRIPTS}/functions
      fi

      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
    '';

    shellAliases = {
      ls = "exa -l";
      refresh = "exec $SHELL -l";
    };
  };
}
