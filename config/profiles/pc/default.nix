{ pkgs, ... }:

{
  imports = [
    ../physical-machine

    ../../modules/security/gpg

    ../../modules/system/devices/bluetooth

    ../../modules/services/docker
    ../../modules/services/postgresql
    ../../modules/services/redis

    ../../modules/ui/audio
    ../../modules/ui/direnv
    ../../modules/ui/dunst
    ../../modules/ui/fonts
    ../../modules/ui/opengl
    ../../modules/ui/picom
    ../../modules/ui/termonad
    ../../modules/ui/xserver

    # TODO: Write Modules:
    # ../../modules/ui/emacs
    # ../../../modules/nixos/lightlocker.nix
    # ../../modules/ui/zathura # Home Manager
  ];

  #nixpkgs.overlays = [ (import ../../../overlays/graphqurl.nix) ];

  environment.systemPackages = with pkgs; [
    # CLI Tools
    ispell
    udiskie
    sqlite # for org-roam, should I move to postgres? How would I sync between machines in that case?
    xclip

    filezilla

    # Editors
    emacs

    # Desktop Environment
    batsignal
    brightnessctl
    brightness-bar
    dunst
    dmenu
    libnotify
    networkmanagerapplet
    trayer
    volume-bar
    xbanish
    xlayoutdisplay

    # Media
    vlc
    scrot
    zathura
    feh

    # Chat/Communication
    slack
    zoom-us
    discord

    # Pandoc Related
    pandoc
    texlive.combined.scheme-full
    python37Packages.pygments
    poppler_utils

    # Web Browsers
    firefox
    google-chrome
  ];

  primary-user.extraGroups = [ "networkmanager" ];
}
