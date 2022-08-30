{ pkgs, ... }:

{
  imports = [
    ../../../modules

    ../../modules/data/session-vars

    ../../modules/security/gpg
    #../../modules/security/process-information-hiding
    ../../modules/security/sshd

    ../../modules/system/nix-binary-caches
    ../../modules/system/nixpkgs
    ../../modules/system/systemd-boot
    ../../modules/system/timezone
    ../../modules/system/users

    # ../../modules/ui/bash       # Home Manager
    ../../modules/services/tailscale

    ../../modules/ui/git
    ../../modules/ui/lorri
    ../../modules/ui/starship
    ../../modules/ui/zsh
  ];

  system.stateVersion = "22.05";
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
    inetutils
    jq
    pass
    ripgrep
    tmux
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
