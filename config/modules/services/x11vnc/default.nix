{ config, lib, pkgs, ... }:

let
  cfg = config.services.x11vnc;
in
{
  options.services.x11vnc = {
    enable = lib.mkEnableOption "x11vnc server for remote desktop access";

    display = lib.mkOption {
      type = lib.types.str;
      default = ":0";
      description = "X display to share";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 5900;
      description = "VNC port to listen on";
    };

    viewOnly = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Allow viewing only, no mouse/keyboard input";
    };

    localhost = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Only listen on localhost (use SSH tunnel or Tailscale to connect)";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra arguments to pass to x11vnc";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.x11vnc ];

    systemd.user.services.x11vnc = {
      description = "x11vnc VNC server";
      after = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = lib.concatStringsSep " " ([
          "${pkgs.x11vnc}/bin/x11vnc"
          "-display ${cfg.display}"
          "-rfbport ${toString cfg.port}"
          "-forever"
          "-shared"
          "-noxdamage"
          "-nopw"
        ]
        ++ lib.optional cfg.viewOnly "-viewonly"
        ++ lib.optional cfg.localhost "-localhost"
        ++ cfg.extraArgs);
        Restart = "on-failure";
        RestartSec = 5;
      };
    };

    # Firewall is managed per-machine (e.g. Tailscale-only access)
    # rather than opened globally here.
  };
}
