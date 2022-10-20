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

      autoload edit-command-line
      zle -N edit-command-line 
      bindkey “^x^e” edit-command-line

      # Load Functions
      if [ -f ${config.primary-user.home-manager.home.sessionVariables.SCRIPTS}/functions ]; then
          source ${config.primary-user.home-manager.home.sessionVariables.SCRIPTS}/functions
      fi

      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '

      function jump-to-git-root {
        local ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null)"
        [[ -z $ROOT_DIR ]] || [[ $ROOT_DIR == $(pwd) ]] || cd $ROOT_DIR
      }
    '';

    shellAliases = let
      home = config.primary-user.home-manager.home.homeDirectory;
       in {
         ls = "exa";
         refresh = "exec $SHELL -l";
         g = "cd \"${home}/Development/$(find ${home}/Development/ -maxdepth 2 -type d  | sed -E 's|^${home}/Development/||g' | fzf)\"";
         gr = "jump-to-git-root";
       };
  };
}
