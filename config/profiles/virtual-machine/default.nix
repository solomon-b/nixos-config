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

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.stateVersion = "22.05";
  nix.settings.trusted-users = [ "@wheel" ];
  environment.shells = [ pkgs.zsh pkgs.bashInteractive ];

  environment.systemPackages = with pkgs; [
    # General CLI Tools
    cachix
    direnv
    fzf
    git
    gnugrep
    htop
    inetutils
    jq
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
