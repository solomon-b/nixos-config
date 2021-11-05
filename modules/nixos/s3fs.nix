{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.programs.s3fs;
  #passwords = pkgs.callPackage ../../lib/passwords.nix { };
in
{
  options.programs.s3fs = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable s3fs-fuse";
    };
  };

  config = mkIf cfg.enable {
    #deployment.keys = {
    #  s3fs-api-token = {
    #    keyCommand = passwords.getPassword "system/absolution-gap/api-token";
    #    destDir = "/secrets";
    #    user = config.primary-user.name;
    #    group = "users";
    #  };
    #};

    environment.systemPackages = [ pkgs.s3fs ];
    environment.variables.AWS_CREDENTIAL_FILE = /secrets/absolution-gap-api-token; #config.deployment.keys.s3fs-api-token.path;

    systemd.user.services.s3fs =
      {
        description = "Mount S3fs drive";
        wantedBy = [ "detect-online.service" ];
        after = [ "detect-online.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${pkgs.s3fs.outPath}/bin/s3fs absolution-gap /home/${config.primary-user.name}/mnt -o url=https://sfo3.digitaloceanspaces.com -o use_path_request_style -o passwd_file=/secrets/s3fs-api-token -o endpoint=sfo3";
          ExecStop = "fusermount -u /home${config.primary-user.name}/mnt";
        };
      };

    systemd.user.services.detect-online =
      {
        description = "Detects if we have internet access";
        wantedBy = [ "default.target"];
        script =
          ''
            PATH="/run/wrappers/bin:$PATH"
            host="''${1:-8.8.8.8}"

            pingcheck() {
              ping -n -c 1 -w 5 $1 >/dev/null 2>&1
            }

            # Do you want a timeout ?
            while :; do
              pingcheck ''${host} && exit 0
              sleep 10
            done
          '';
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
      };
  };
}
