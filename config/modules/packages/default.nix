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

    # Custom scripts
    (pkgs.writeShellApplication {
      name = "zfs-status";
      runtimeInputs = with pkgs; [ zfs openssh systemd coreutils gum ];
      text = ''
        # Set default environment variables
        export ZFS_STATUS_DEST_HOST=''${ZFS_STATUS_DEST_HOST:-"sandra-voi.home.arpa"}
        export ZFS_STATUS_DEST_USER=''${ZFS_STATUS_DEST_USER:-"solomon"}
        export ZFS_STATUS_DEST_BASE=''${ZFS_STATUS_DEST_BASE:-"tank/system-snapshots"}
        
        ${builtins.readFile ../../../scripts/zfs-status.sh}
      '';
    })
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