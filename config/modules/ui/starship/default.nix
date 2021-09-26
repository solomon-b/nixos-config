{ config, ... }:

{
  config.primary-user.home-manager.programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = false;
      character = {
        success_symbol = "➜";
        error_symbol = "✗";
      };
      line_break.disabled = true;
    };
  };
}
