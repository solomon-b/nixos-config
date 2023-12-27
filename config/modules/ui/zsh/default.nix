{ pkgs, lib, config, ... }:

let
  home = config.primary-user.home-manager.home.homeDirectory;
in
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

      function git-jump {
        REPO=$(find ${home}/Development/ -maxdepth 3 -type d -exec test -e '{}/.git' ';' -print -prune | sed -E 's|^${home}/Development/||g' | fzf)
        [[ -n $REPO ]] && cd "${home}/Development/''${REPO}"
      }

      function jump-to-git-root {
        local ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null)"
        [[ -z $ROOT_DIR ]] || [[ $ROOT_DIR == $(pwd) ]] || cd $ROOT_DIR
      }

      function fzf-checkout {
          BRANCH=$(git branch | fzf| tr -d '[:space:]')
          [[ -n $BRANCH ]] && git checkout $BRANCH
      }
      
      function git-checkout-branch {
          $(git rev-parse --is-inside-work-tree >& /dev/null)
      
          if [ "$?" -eq "0" ]; then
              [[ -n $1 ]] && git checkout $1 || fzf-checkout
          fi
      }
    '';

    shellAliases = {
      refresh = "exec $SHELL -l";
      g = "git-jump";
      gr = "jump-to-git-root";
      gp = "git pull";
      gf = "git fetch";
      gc = "git-checkout-branch";
    };
  };
}
