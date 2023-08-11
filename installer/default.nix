# https://github.com/Gerg-L/nixos/blob/master/installer/default.nix
{ config, pkgs, lib, system ? builtins.currentSystem, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/profiles/minimal.nix"
    "${modulesPath}/installer/cd-dvd/installation-cd-base.nix"
  ];

  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];

  users = {
    mutableUsers = false;
    users.root.openssh.authorizedKeys.keys =
      import ../config/modules/security/sshd/public-keys.nix;
  };

  isoImage = {
    edition = lib.mkForce "custom";
    isoName = lib.mkForce "NixOS.iso";
  };

  nix = {
    settings = {
      experimental-features = ["nix-command" "flakes" "repl-flake"];
      auto-optimise-store = true;
    };
  };

  environment = {
    systemPackages = [
      pkgs.gitMinimal
      pkgs.vim
    ];

    etc = {
      "install-server.sh" = {
        source = ./install-server.sh;
        mode = "0700";
      };

      "configuration.nix" = {
        source = ./configuration.nix;
        mode = "0600";
      };
    };
  };
}
