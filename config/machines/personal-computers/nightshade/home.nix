{ config, pkgs, lib, ... }:

{
  imports = [
    ../../../../modules/home-manager/primary-user.nix
    ../../../modules/ui/dmenu/home.nix
    ../../../modules/ui/emacs/home.nix
    ../../../modules/ui/direnv/home.nix
    ../../../modules/ui/dunst/home.nix
    ../../../modules/ui/st/home.nix
    ../../../modules/ui/starship/home.nix
    ../../../modules/ui/zsh/home.nix
    ../../../modules/ui/git/home.nix
    ../../../modules/ui/ntfy/home.nix
    ../../../modules/packages/pc-cli-tools.nix
    ../../../modules/packages/gui-applications.nix
    ../../../modules/packages/development.nix
    ./kmonad.nix
  ];

  programs.bash.enable = true;
  targets.genericLinux.enable = true;

  services.ssh-agent.enable = true;

  home.packages = with pkgs; [
    # Machine-specific packages not in the shared modules
    acpi
    curl
    xterm
    eww
    kmonad
    stix-two
    (agda.withPackages (p: [ p._1lab p.standard-library ]))
  ];

  home.sessionVariables = {
    EDITOR = "vim";
  };

  xdg.configFile."startup.sh".text = ''
    ${pkgs.feh}/bin/feh --bg-scale ${../../../modules/ui/xserver/wallpapers/Yosemite-Color-Block.png}
  '';
}
