{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.services.micasa;
in
{
  options.services.micasa = {
    enable = mkEnableOption "micasa SSH service";

    package = mkOption {
      type = types.package;
      description = "The micasa package to use.";
    };

    user = mkOption {
      type = types.str;
      default = "micasa";
      description = "The user account for the micasa SSH service.";
    };

    authorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "SSH public keys authorized to access micasa.";
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.user} = {
      isNormalUser = true;
      shell = pkgs.bashInteractive;
      openssh.authorizedKeys.keys = cfg.authorizedKeys;
    };

    services.openssh.extraConfig = mkAfter ''
      Match User ${cfg.user}
        ForceCommand ${cfg.package}/bin/micasa
        AllowTcpForwarding no
        X11Forwarding no
    '';
  };
}
