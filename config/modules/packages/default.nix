{ pkgs, ... }:

{
  server-cli-tools = with pkgs; [
    btop
    dysk
    fd
    fzf
    gum
    jq
    ripgrep
    sqlite
    sysz
    tmux
    tree
    unzip
    wget

    # Editors
    vim-full

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

  pc-cli-tools = with pkgs; [
    # General
    btop
    dysk
    fd
    fzf
    gum
    ispell
    jq
    nix-search
    postgresql
    ranger
    ripgrep
    sqlite
    sysz
    tmux
    tree
    udiskie
    unzip
    wget

    # AI
    # claude-code

    # Editors
    vim-full

    # File Management
    filezilla

    # Document Processing
    pandoc
    texlive.combined.scheme-full
    poppler-utils

    # Desktop Environment Utilities
    xclip
    xdotool
    xorg.xkill
    brightnessctl
    libnotify
    #xlayoutdisplay
    wmctrl

    # Media Tools
    scrot

    # Desktop Secrets
    pinentry-gtk2

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
    # surf

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
    vscodium

    # CAD
    kicad
  ];
}
