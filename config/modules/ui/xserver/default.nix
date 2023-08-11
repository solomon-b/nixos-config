{ pkgs, ... }:

{
  services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "ctrl:nocaps";
      xautolock = {
        time = 15;
        enable = true;
      };
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
        lightdm.background = ./wallpapers/Yosemite-Color-Block.png;
      };
  };

  environment.systemPackages = [
    pkgs.haskellPackages.xmonad-solomon
    pkgs.haskellPackages.xmobar-solomon
  ];

  # https://mynixos.com/home-manager/option/xdg.configFile.%3Cname%3E.source
  primary-user.home-manager.xdg.configFile."startup.sh".text = ''
    ${pkgs.networkmanagerapplet}/bin/nm-applet
    ${pkgs.feh}/bin/feh --bg-scale ${./wallpapers/Yosemite-Color-Block.png}
    ${pkgs.xbanish}/bin/xbanish
    ${pkgs.trayer}/bin/trayer --edge top --width 3 --align right --height 17 --transparent true --alpha 0 --tint 0x2d2d2d
    ${pkgs.dunst}/bin/dunst
    ${pkgs.batsignal}/bin/batsignal -b -W \"Warnings: Battery Low\"
    ${pkgs.volume-bar}/bin/volume-bar
    ${pkgs.brightness-bar}/bin/brightness-bar
  '';

  primary-user.home-manager.xdg.dataFile."emoji".source = ./emoji;
}
