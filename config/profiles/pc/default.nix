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
    ../../modules/ui/dmenu
    ../../modules/ui/fonts
    ../../modules/ui/opengl
    ../../modules/ui/picom
    ../../modules/ui/st
    ../../modules/ui/xserver

    ./openai-codex.nix
  ];

  #nixpkgs.overlays = [ (import ../../../overlays/graphqurl.nix) ];

  # TODO: Move user packages into home-manager:
  environment.systemPackages = with pkgs; [
    # CLI Tools
    claude-code
    fd
    ispell
    udiskie
    sqlite # for org-roam, should I move to postgres? How would I sync between machines in that case?
    xclip
    xdotool
    xorg.xkill
    ranger
    nix-search

    filezilla

    # Editors
    emacs
    vscodium

    # Desktop Environment
    brightnessctl
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
    pinentry-gtk2

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
