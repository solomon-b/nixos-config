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

  primary-user.home-manager = {
    imports = [
      ../../modules/packages/pc-cli-tools.nix
      ../../modules/packages/gui-applications.nix
      ../../modules/packages/development.nix
      ../../modules/ui/ntfy/home.nix
    ];
  };

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
