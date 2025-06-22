{ pkgs, ... }:

{
  imports = [
    ../../../modules

    ../../modules/data/session-vars

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

  primary-user.name = "solomon";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  primary-user.home-manager = {
    imports = [
      ../../modules/packages/server-cli-tools.nix
    ];
  };

  system.stateVersion = "22.05";
  nix.settings.trusted-users = [ "@wheel" ];
  environment.shells = [ pkgs.zsh pkgs.bashInteractive ];

  environment.systemPackages = with pkgs; [
    # System-level tools needed for NixOS functionality
    cachix
    direnv
    git
    gnugrep
    inetutils
    zlib
    zsh
    zsh-syntax-highlighting
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  services.prometheus.exporters = {
    node = {
      enable = true;
      enabledCollectors = [ "systemd" ];
      port = 9002;
    };
  };

  networking.firewall.allowedTCPPorts = [ 9002 ];
}
