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
    options = [
      "defaults"                  # → rw,suid,dev,exec,auto,nouser,async
      "vers=3"                    # force NFSv3
      "proto=tcp"                 # use TCP transport
      "intr"                      # allow signals (Ctrl-C) to interrupt
      "timeo=30"                  # initial timeout = 3 s (30 deciseconds)
      "retrans=3"                 # retry only 3 times (~9 s total)
      "_netdev"                   # wait for network before mounting
    ];
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
    ];

    authentication = ''
      host all all 100.76.234.131/32 md5
      host all all zodiacal-light md5
      host all all 192.168.5.7/32 md5

      host all all 100.100.33.33/32 md5
      host all all nightshade md5

      host tt_rss tt_rss accompaniment-of-shadows md5

      host immich immich 100.80.98.4/32 md5
      host immich immich 192.168.5.7/32 md5
      host immich immich 192.168.1.131/32 md5
      host immich immich sower md5

      host hass hass 100.80.98.4/32 md5
      host hass hass 192.168.5.7/32 md5
      host hass hass sower md5

      host planka planka_admin 100.123.147.26/32 md5
      host planka planka_admin 192.168.5.105/32 md5
      host planka planka_admin accompaniment-of-shadows md5
    '';
  };

  networking.firewall.allowedTCPPorts = [ 5432 6379 ];
}
