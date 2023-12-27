{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.zsh;
  autoSuggestionsModule = types.submodule ({ config, ... }: {
    options.highlightStyle = mkOption {
      type = types.str;
      default = "fg=8";
      description = "Configure the style that the suggestion is shown with.";
    };
  });
  historySubStringModule = types.submodule ({ config, ... }: {
    options = {
      highlightFound = mkOption {
        type = types.str;
        default = "bg=magenta,fg=white,bold";
        description = ''
          Defines how the query should be highlighted inside a matching command. Its default value causes this script to highlight using bold, white text on a magenta background. See the "Character Highlighting" section in the zshzle(1) man page to learn about the kinds of values you may assign to this variable.
        '';
      };

      highlightNotFound = mkOption {
        type = types.str;
        default = "bg=red,fg=white,bold";
        description = ''
          Defines how the query should be highlighted when no commands in the history match it. Its default value causes this script to highlight using bold, white text on a red background. See the "Character Highlighting" section in the zshzle(1) man page to learn about the kinds of values you may assign to this variable.
        '';
      };

      globbingFlags = mkOption {
        type = types.str;
        default = "i";
        description = ''
          defines how the command history will be searched for your query. Its default value causes this script to perform a case-insensitive search. See the "Globbing Flags" section in the zshexpn(1) man page to learn about the kinds of values you may assign to this variable.
        '';
      };

      ensureUnique = mkOption {
        type = types.str;
        default = "";
        description = ''
          Defines whether all search results returned are unique. If set to a non-empty value, then only unique search results are presented.
        '';
      };

      fuzzy = mkOption {
        type = types.str;
        default = "";
        description = ''
          Defines how the command history will be searched for your query. If set to a non-empty value, causes this script to perform a fuzzy search by words, matching in given order e.g. ab c will match *ab*c*.
        '';
      };
    };
  });
in
{
  options.programs.zsh = {
    enableHistorySubstringSearch = mkOption {
      type = types.bool;
      default = false;
      description = "Enable zsh history substring search";
    };

    historySubstring = mkOption {
      type = historySubStringModule;
      default = { };
      description = "Options related to History Substring Search";
    };

    autoSuggestions = mkOption {
      type = autoSuggestionsModule;
      default = { };
      description = "Options related to Autosuggestions";
    };

    saveNoDups = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Don't write duplicate entries in the history file. Defaults to false.
      '';
    };
  };


  config = mkIf cfg.enable {
    home.file.".zshrc".text = ''
      ${optionalString cfg.enableHistorySubstringSearch
        "source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
      }

      ${if cfg.enableAutosuggestions
        then "ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='${cfg.autoSuggestions.highlightStyle}'"
        else ""}

      ${if cfg.enableHistorySubstringSearch
        then "typeset -g HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='${cfg.historySubstring.highlightFound}'"
        else ""}

      ${if cfg.enableHistorySubstringSearch
        then "typeset -g HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='${cfg.historySubstring.highlightNotFound}'"
        else ""}

      ${if cfg.enableHistorySubstringSearch
        then "typeset -g HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='${cfg.historySubstring.globbingFlags}'"
        else ""}

      ${if cfg.enableHistorySubstringSearch
        then "typeset -g HISTORY_SUBSTRING_SEARCH_FUZZY='${cfg.historySubstring.fuzzy}'"
        else ""}

      ${if cfg.enableHistorySubstringSearch
        then "typeset -g HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE='${cfg.historySubstring.ensureUnique}'"
        else ""}
      ${if cfg.saveNoDups
        then "setopt HIST_SAVE_NO_DUPS"
        else ""}
    '';
  };
}
