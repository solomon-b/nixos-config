{ pkgs, ... }:

{
  services.displayManager = {
    defaultSession = "none+xmonad";
  };

  services.displayManager = {
    ly.enable = true;
  };
  services.xserver = {
    enable = true;
    xkb = {
      layout = "us";
      options = "ctrl:nocaps";
    };
    xautolock = {
      time = 15;
      enable = false;
      locker = "${pkgs.betterlockscreen}/bin/betterlockscreen --lock";
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
    # The names of the video drivers the configuration supports. They will be
    # tried in order until one that supports your card is found. Don’t combine
    # those with “incompatible” OpenGL implementations, e.g. free ones
    # (mesa-based) with proprietary ones.
    videoDrivers = [
      "nvidia"
      "modesetting"
      "fbdev"
    ];
  };

  systemd.services.nvidia-control-devices = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
  };

  environment.systemPackages = [
    pkgs.haskellPackages.xmonad-solomon
  ];

  # https://mynixos.com/home-manager/option/xdg.configFile.%3Cname%3E.source
  primary-user.home-manager.xdg.configFile."startup.sh".text = ''
    ${pkgs.networkmanagerapplet}/bin/nm-applet
    ${pkgs.feh}/bin/feh --bg-scale ${./wallpapers/Yosemite-Color-Block.png}
    ${pkgs.xbanish}/bin/xbanish
    ${pkgs.trayer}/bin/trayer --edge top --align right --widthtype percent --width 5 --height 17 --transparent true --alpha 0 --tint 0x2d2d2d
    ${pkgs.dunst}/bin/dunst
    ${pkgs.batsignal}/bin/batsignal -b -W \"Warnings: Battery Low\"
    ${pkgs.volume-bar}/bin/volume-bar
    ${pkgs.brightness-bar}/bin/brightness-bar
    # ${pkgs.eww}/bin/eww open ewwbar
  '';

  primary-user.home-manager.xdg.dataFile."emoji".source = ./emoji;
}
