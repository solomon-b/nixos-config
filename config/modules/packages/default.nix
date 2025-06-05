{ pkgs, ... }:

{
  cli-tools = with pkgs; [
    # CLI Tools
    claude-code
    fd
    ispell
    udiskie
    sqlite
    xclip
    xdotool
    xorg.xkill
    ranger
    nix-search

    # File Management
    filezilla

    # Database
    postgresql

    # Pandoc Related
    pandoc
    texlive.combined.scheme-full
    poppler_utils

    # Desktop Environment Utilities
    brightnessctl
    libnotify
    xlayoutdisplay
    wmctrl

    # Media Tools
    scrot

    # Secrets
    pinentry-gtk2

    # General CLI Tools (moved from physical-machine profile)
    dysk
    fzf
    gum
    btop
    jq
    ripgrep
    sysz
    tmux
    tree
    unzip
    wget

    # Editors
    vimHugeX
  ];

  gui-applications = with pkgs; [
    # Web Browsers
    firefox
    google-chrome
    surf

    # Chat/Communication
    discord
    signal-desktop
    slack
    telegram-desktop
    zoom-us

    # Media
    feh
    pavucontrol
    picard
    vlc
    zathura

    # Secrets
    yubioath-flutter

    # Misc Graphical Tools
    zotero
  ];

  development = with pkgs; [
    # Editors
    emacs
    vscodium
  ];
}