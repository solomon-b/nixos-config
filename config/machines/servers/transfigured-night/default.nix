# Postgres Service
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
  ];

  networking.hostName = "transfigured-night";

  fileSystems."/mnt/postgresql" = {
    device = "192.168.5.6:/mnt/tank/postgresql";
    fsType = "nfs";
  };

  services.postgresql = {
    enable = true;
    dataDir = "/mnt/postgresql";
    enableTCPIP = true;
    package = pkgs.postgresql_14;

    ensureDatabases = [
      "tt_rss"
      "hasura"
    ];

    ensureUsers = [
      { name = "tt_rss";
        ensurePermissions = {
          "DATABASE tt_rss" = "ALL PRIVILEGES";
        };
      }
      { name = "hasura_admin";
        ensurePermissions = {
          "DATABASE hasura" = "ALL PRIVILEGES";
        };
      }
    ];

    authentication = ''
      host all all nightshade md5
      host tt_rss tt_rss accompaniment-of-shadows md5
    '';
  };

  networking.firewall.allowedTCPPorts = [ 5432 ];
}
