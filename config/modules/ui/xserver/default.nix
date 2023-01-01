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

  environment.systemPackages = [
    pkgs.haskellPackages.xmonad
    pkgs.haskellPackages.xmonad-solomon
    pkgs.haskellPackages.xmobar-solomon
    #pkgs.haskellPackages.taffybar-solomon
  ];

  # https://mynixos.com/home-manager/option/xdg.configFile.%3Cname%3E.source
  primary-user.home-manager.xdg.configFile."startup.sh".text = ''
    nm-applet
    feh --bg-scale /home/solomon/Public/wallpapers/Yosemite-Color-Block.png
    xbanish
    trayer --edge top --width 3 --align right --height 17 --transparent true --alpha 0 --tint 0x2d2d2d
    dunst
    batsignal -b -W \"Warnings: Battery Low\"
    volume-bar
    brightness-bar
    sleep 2 && kmonad /home/solomon/.config/kmonad.kbd
  '';

  primary-user.home-manager.xdg.dataFile."emoji".source = ./emoji;
}
