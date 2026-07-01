{ config, ... }:

{
  fileSystems."/mnt/paperless-ngx" = {
    device = "192.168.5.6:/mnt/tank/app-data/paperless-ngx";
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

  services.paperless = {
    enable = true;
    consumptionDir = "/mnt/paperless-ngx/consume";
    mediaDir = "/mnt/paperless-ngx/media";
    passwordFile = config.sops.secrets.paperless-password.path;
    port = 9000;
    settings = {
      PAPERLESS_CONSUMER_POLLING = 30;
      # Keep these OFF /var/tmp: PrivateTmp=yes gives each paperless service a
      # private /var/tmp, and systemd-tmpfiles-clean ages it out (30d), deleting
      # SCRATCH_DIR from under the long-running worker -> consume_file fails with
      # FileNotFoundError. /var/lib/paperless is the persistent StateDirectory.
      PAPERLESS_CONVERT_TMPDIR = "/var/lib/paperless/tmp";
      PAPERLESS_SCRATCH_DIR = "/var/lib/paperless/scratch";
    };
  };

  # Guarantee the scratch/convert temp dirs exist and are never garbage-collected.
  # They live under the persistent StateDirectory and are owned by paperless.
  systemd.tmpfiles.rules = [
    "d /var/lib/paperless/tmp 0700 paperless paperless -"
    "d /var/lib/paperless/scratch 0700 paperless paperless -"
  ];

  sops.secrets = {
    paperless-password = { };
  };

  services.nginx.virtualHosts."paperless.service.home.arpa" = {
    locations."/".proxyPass = "http://localhost:9000";
  };
}
