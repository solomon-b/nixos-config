{ config, pkgs, lib, ... }:

{
  imports = [
    ../../../../modules/home-manager/primary-user.nix
    ../../../modules/ui/emacs/home.nix
    ../../../modules/ui/direnv/home.nix
    ../../../modules/ui/dunst/home.nix
    ../../../modules/ui/st/home.nix
    ../../../modules/ui/starship/home.nix
    ../../../modules/ui/zsh/home.nix
    ../../../modules/ui/git/home.nix
    ./kmonad.nix
  ];

  programs.bash.enable = true;
  targets.genericLinux.enable = true;

  home.packages = with pkgs; [
    # General CLI Tools
    aider-chat
    acpi
    cachix
    curl
    direnv
    fzf
    git
    gnugrep
    gnumake
    gum
    btop
    inetutils
    jq
    #pass
    ripgrep
    sysz
    tmux
    tree
    unzip
    wget
    zlib
    zsh
    zsh-syntax-highlighting

    # Editors
    vimHugeX

    # CLI Tools
    fd
    ispell
    udiskie
    sqlite # for org-roam, should I move to postgres? How would I sync between machines in that case?
    xclip
    ranger

    filezilla

    # Editors
    #emacs
    vscodium

    # Desktop Environment
    xterm
    brightnessctl
    eww
    kmonad
    libnotify
    trayer
    wmctrl
    xlayoutdisplay

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

    # Misc Graphical Tools
    zotero

    (agda.withPackages (p: [ p._1lab p.standard-library ]))
  ];

  home.sessionVariables = {
    EDITOR = "vim";
  };
}
