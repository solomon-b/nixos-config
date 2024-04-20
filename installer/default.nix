# https://github.com/Gerg-L/nixos/blob/master/installer/default.nix
{ config, pkgs, lib, system ? builtins.currentSystem, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
  ];

  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];

  users = {
    mutableUsers = false;
    users.root.openssh.authorizedKeys.keys =
      import ../config/modules/security/sshd/public-keys.nix;
  };

  isoImage = {
    edition = lib.mkForce "solomon-min";
    isoName = lib.mkForce "NixOS.iso";
  };

  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      auto-optimise-store = true;
    };
  };

  environment = {
    systemPackages = [ pkgs.vim pkgs.rsync ];

    etc = {
      "connect.sh" = {
        source = ./connect.sh;
        mode = "0777";
      };

      "configuration.nix" = {
        source = ./configuration.nix;
        mode = "0666";
      };
    };
  };
}
