{ pkgs, ... }:

{
  imports = [
    ../../../modules

    ../../modules/data/session-vars

    ../../modules/security/gpg
    ../../modules/security/sshd

    ../../modules/system/nix-binary-caches
    ../../modules/system/nixpkgs
    ../../modules/system/systemd-boot
    ../../modules/system/timezone
    ../../modules/system/users

    ../../modules/services/tailscale

    ../../modules/ui/git
    ../../modules/ui/starship
    ../../modules/ui/zsh
  ];

  system.stateVersion = "22.05";
  nix.settings.trusted-users = [ "@wheel" ];
  environment.shells = [ pkgs.zsh pkgs.bashInteractive ];

  primary-user.home-manager = {
    imports = [
      ../../modules/packages/server-cli-tools.nix
    ];
  };

  environment.systemPackages = with pkgs; [
    # System-level tools needed for NixOS functionality
    cachix
    direnv
    git
    gnugrep
    gnumake
    inetutils
    pass
    zlib
    zsh
    zsh-syntax-highlighting
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
}
