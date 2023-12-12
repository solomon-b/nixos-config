{ ... }:

{
  primary-user.home-manager.programs.kitty = {
    enable = true;
    shellIntegration = {
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
  };

  primary-user.home-manager.xdg.configFile = {
    "kitty/current-theme.conf".source = ./current-theme.conf;
    "kitty/kitty.conf".source = ./kitty.conf;
  };
}
