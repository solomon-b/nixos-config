# Postgres and Redis Service
{ pkgs, lib, ... }:

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

  services.redis.servers = {
    immich = {
      enable = true;
      openFirewall = true;
      port = 6379;
      # TODO: Move into SOPS
      requirePass = "hunter2";
      bind = null;
    };

    tubearchivist = {
      enable = true;
      openFirewall = true;
      port = 6380;
      # TODO: Move into SOPS
      requirePass = "hunter2";
      bind = null;
    };
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
      host all all 100.100.33.33/32 md5
      host all all nightshade md5
      host tt_rss tt_rss accompaniment-of-shadows md5
      host immich immich 100.80.98.4/32 md5
      host immich immich 192.168.5.7/32 md5
      host immich immich sower md5
    '';
  };

  networking.firewall.allowedTCPPorts = [ 5432 6379 ];
}
