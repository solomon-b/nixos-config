# PostgreSQL Service
{ config, pkgs, lib, ... }:

{
  # Mount PostgreSQL data from NFS
  fileSystems."/mnt/postgresql" = {
    device = "192.168.5.6:/mnt/tank/postgresql";
    fsType = "nfs";
    options = [
      "defaults" # → rw,suid,dev,exec,auto,nouser,async
      "vers=3" # force NFSv3
      "proto=tcp" # use TCP transport
      "intr" # allow signals (Ctrl-C) to interrupt
      "timeo=30" # initial timeout = 3 s (30 deciseconds)
      "retrans=3" # retry only 3 times (~9 s total)
      "_netdev" # wait for network before mounting
    ];
  };

  services.postgresql = {
    enable = true;
    dataDir = "/mnt/postgresql";
    enableTCPIP = true;
    extraPlugins = with pkgs.postgresql_14.pkgs; [ pgvecto-rs ];
    package = pkgs.postgresql_14;

    settings = {
      shared_preload_libraries = "vectors.so";
    };

    ensureDatabases = [
      "tt_rss"
      "planka"
      "immich"
      "hass"
      "irrigation"
    ];

    ensureUsers = [
      {
        name = "tt_rss";
        ensureDBOwnership = true;
      }
      {
        name = "planka_admin";
      }
      {
        name = "immich";
        ensureDBOwnership = true;
      }
      {
        name = "hass";
        ensureDBOwnership = true;
      }
      {
        name = "irrigation";
        ensureDBOwnership = true;
      }
    ];

    authentication = ''
      # Local connections from accompaniment-of-shadows services
      host tt_rss tt_rss 127.0.0.1/32 md5
      host tt_rss tt_rss localhost md5
      host planka planka_admin 127.0.0.1/32 md5
      host planka planka_admin localhost md5

      # Remote connections from sower (Immich, Home Assistant)
      host immich immich 100.80.98.4/32 md5
      host immich immich 192.168.5.7/32 md5
      host immich immich 192.168.1.131/32 md5
      host immich immich sower md5

      host hass hass 100.80.98.4/32 md5
      host hass hass 192.168.5.7/32 md5
      host hass hass sower md5

      host irrigation irrigation 100.80.98.4/32 md5
      host irrigation irrigation 192.168.5.7/32 md5
      host irrigation irrigation sower md5

      # Admin access from personal computers
      host all all 100.76.234.131/32 md5
      host all all zodiacal-light md5
      host all all 100.100.33.33/32 md5
      host all all nightshade md5
      host all all 100.126.171.15/32 md5
      host all all lorean md5
      host all all 192.168.5.7/32 md5
    '';
  };

  networking.firewall.allowedTCPPorts = [ 5432 ];
}
