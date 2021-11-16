{ pkgs, ... }:

{
  imports = [
    ../../../modules

    ../../modules/data/session-vars

    ../../modules/security/gpg
    #../../modules/security/process-information-hiding
    ../../modules/security/sshd
    # ../../modules/security/sudo  # Ask Connor

    ../../modules/system/nix-binary-caches
    ../../modules/system/nixpkgs
    ../../modules/system/users

    # ../../modules/ui/bash       # Home Manager
    ../../modules/ui/starship
    ../../modules/ui/zsh

    # TODO: Write modules:
    # ../../../modules/nixos/termonad.nix
  ];

  nix.trustedUsers = [ "@wheel" ];
  environment.shells = [pkgs.zsh pkgs.bashInteractive];

  environment.systemPackages = with pkgs; [
    # General CLI Tools
    cachix
    direnv
    exa
    fzf
    git
    gnugrep
    gnumake
    htop
    jq
    nixops
    ripgrep
    tree
    unzip
    wget
    zlib
    zsh
    zsh-syntax-highlighting

    # Editors
    vimHugeX
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
}