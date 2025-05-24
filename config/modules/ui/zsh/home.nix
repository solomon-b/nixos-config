{ pkgs, lib, config, ... }:

{
  programs.zsh = {
    dotDir = ".config/zsh";
    enable = true;
    autosuggestion.enable = true;

    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
      save = 10000;
      share = true;
      size = 50000;
    };

    historySubstringSearch.enable = true;

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

    initContent = ''
             function git-jump() {
               local REPO
               REPO=$(fd -H -t d '.git' ${config.home.homeDirectory}/Development -d 5 -E '.github' | sed 's|/\.git/$||' | awk '{ print; system("dirname \"" $0 "\"") }' | sed 's|^${config.home.homeDirectory}/Development/||' | sort -u | fzf --height 25)

               if [[ -n $REPO ]]; then
                 local DEST="${config.home.homeDirectory}/Development/''${REPO}"

               cd "$DEST" || return
               # Refresh prompt after changing directory
               zle reset-prompt

               fi
             }

            zle -N git-jump

            function jump-to-git-root {
              local ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null)"
              [[ -z $ROOT_DIR ]] || [[ $ROOT_DIR == $(pwd) ]] || cd $ROOT_DIR
            }

            function fzf-checkout {
                BRANCH=$(git branch | fzf --height 25| tr -d '[:space:]')
                [[ -n $BRANCH ]] && git checkout $BRANCH
            }

            function git-checkout-branch {
                $(git rev-parse --is-inside-work-tree >& /dev/null)

                if [ "$?" -eq "0" ]; then
                    [[ -n $1 ]] && git checkout $1 || fzf-checkout
                fi
            }

            bindkey '^l' autosuggest-accept
            bindkey '^h' autosuggest-clear
            bindkey '^k' history-substring-search-up
            bindkey '^j' history-substring-search-down
            #bindkey '^R' history-incremental-search-backward
            bindkey '^g' git-jump

            autoload edit-command-line
            zle -N edit-command-line
            bindkey “^x^e” edit-command-line

            # Load Functions
            if [ -f ${config.xdg.configHome}/functions ]; then
                source ${config.xdg.configHome}/functions
            fi
      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
    '';

    shellAliases = {
      cursor = "$HOME/.local/bin/cursor";
      refresh = "exec $SHELL -l";
      g = "git-jump";
      gr = "jump-to-git-root";
      gp = "git pull";
      gf = "git fetch";
      gc = "git-checkout-branch";
      s = "sysz";
      t = "sudo tailscale";

      "~" = "cd ~";
      "/" = "cd /";
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../..";
      "....." = "cd ../../../..";
      "......" = "cd ../../../../..";
      "rmd" = "rm -r";
    };
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}
