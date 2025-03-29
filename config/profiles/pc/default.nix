{ config, pkgs, inputs, ... }:

{
  imports = [
    ../physical-machine

    ../../modules/security/gpg

    ../../modules/system/devices/bluetooth
    ../../modules/system/devices/udisk
    ../../modules/system/nix-direnv

    ../../modules/ui/audio
    ../../modules/ui/direnv
    ../../modules/ui/dunst
    # ../../modules/ui/eww
    ../../modules/ui/fonts
    ../../modules/ui/opengl
    ../../modules/ui/picom
    ../../../modules/home-manager/st
    ../../modules/ui/xserver

    # TODO: Write Modules:
    # ../../modules/ui/emacs
    # ../../../modules/nixos/lightlocker.nix
    # ../../modules/ui/zathura # Home Manager
  ];

  #nixpkgs.overlays = [ (import ../../../overlays/graphqurl.nix) ];

  environment.systemPackages = with pkgs; [
    # CLI Tools
    fd
    ispell
    udiskie
    sqlite # for org-roam, should I move to postgres? How would I sync between machines in that case?
    xclip
    ranger
    nix-search

    filezilla

    # Editors
    emacs
    vscodium

    # Desktop Environment
    brightnessctl
    eww
    libnotify
    xlayoutdisplay
    wmctrl

    # DB
    postgresql

    # Media
    feh
    pavucontrol
    picard
    vlc
    scrot
    zathura

    # Secrets
    yubioath-flutter

    # Chat/Communication
    discord
    signal-desktop
    slack
    telegram-desktop
    zoom-us

    # Pandoc Related
    pandoc
    texlive.combined.scheme-full
    #pythonPackages.pygments
    poppler_utils

    # Web Browsers
    firefox
    google-chrome
    surf

    # Misc Graphical Tools
    zotero
  ];


  primary-user.home-manager.programs.rofi = {
    enable = true;
    #location = "top";
    plugins = [
      #  pkgs.rofi-mpd
      pkgs.rofi-calc
      pkgs.rofi-emoji
      #  pkgs.rofi-systemd
      #  pkgs.rofi-power-menu
    ];
  };

  virtualisation = {
    containers = {
      enable = true;
    };

    docker = {
      enable = true;
      storageDriver = "overlay2";
    };
    oci-containers.backend = "docker";
  };

  primary-user.extraGroups = [ "networkmanager" "docker" ];
}
