{ config, lib, pkgs, ... }:

let
  cfg = config.services.podgrab;
  defaultUser = "podgrab";
  defaultGroup = defaultUser;
in
{
  options.services.podgrab = with lib; {
    enable = mkEnableOption "Podgrab, a self-hosted podcast manager";

    passwordFile = mkOption {
      type = with types; nullOr str;
      default = null;
      example = "/run/secrets/password.env";
      description = ''
        The path to a file containing the PASSWORD environment variable
        definition for Podgrab's authentification.
      '';
    };

    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/${defaultUser}/data";
      example = "/home/yourUser/data";
      description = ''
        The path where Podgrab will save podcast data.
      '';
    };

    port = mkOption {
      type = types.port;
      default = 8080;
      example = 4242;
      description = ''
        "The port on which Podgrab will listen for incoming HTTP traffic.
      '';
    };

    exposePort = mkOption {
      type = types.bool;
      default = false;
      example = true;
      description = ''
        "Open the designated port in the system firewall.
      '';
    };

    user = mkOption {
      type = types.str;
      default = defaultUser;
      example = "yourUser";
      description = ''
        The user to run podgrab as.
        By default, a user named <literal>${defaultUser}</literal> will be created.
      '';
    };

    group = mkOption {
      type = types.str;
      default = defaultGroup;
      example = "yourGroup";
      description = ''
        The group to run Podgrab under.
        By default, a group named <literal>${defaultGroup}</literal> will be created.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = lib.mkIf cfg.exposePort [ cfg.port ];

    users.users = lib.mkIf (cfg.user == defaultUser) {
      ${defaultUser} =
        { group = cfg.group;
          home  = "/var/lib/${cfg.user}";
          createHome = true;
          uid = 899;
          isSystemUser = true;
          description = "Podgrab service user";
        };
    };

    users.groups = lib.mkIf (cfg.group == defaultGroup) {
      ${defaultGroup}.gid = 899;
    };

    systemd.services = {
      podgrab = {
        description = "Podgrab podcast manager";
        wantedBy = [ "multi-user.target" ];

        environment = {
          CONFIG = "/var/lib/${cfg.user}/config";
          DATA = cfg.dataDir;
          GIN_MODE = "release";
          PORT = toString cfg.port;
        };

        serviceConfig = {
          Type = "exec";
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.podgrab}/bin/podgrab";
          EnvironmentFile = lib.optional (cfg.passwordFile != null) [
            cfg.passwordFile
          ];
        };
      };
    };
  };
}
