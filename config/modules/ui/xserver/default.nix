{ pkgs, ... }:

{
  services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "ctrl:nocaps";
      desktopManager.xfce.enable = true;
      windowManager.session = [
        {
          name = "xmonad";
          start = ''
            /usr/bin/env xmonad-solomon &
            waitPID=$!
          '';
        }
      ];
      displayManager = {
        defaultSession = "none+xmonad";
        lightdm.enable = true;
        lightdm.background = "/usr/share/backgrounds/Vaporwave.jpg";
      };
  };

  environment.systemPackages = [ pkgs.xmonad-solomon pkgs.xmobar-solomon ];
}
