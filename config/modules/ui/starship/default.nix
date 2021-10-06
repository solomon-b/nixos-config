{ config, ... }:

{
  config.primary-user.home-manager.programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = {
      add_newline = true;
      character = {
        success_symbol = "➜";
        error_symbol = "✗";
      };
      line_break.disabled = true;
    };
  };
}
