{ config, ... }:

{
  fileSystems."/mnt/paperless-ngx" = {
    device = "192.168.5.6:/mnt/tank/app-data/paperless-ngx";
    fsType = "nfs";
    options = [
      "defaults"                  # → rw,suid,dev,exec,auto,nouser,async
      "vers=3"                    # force NFSv3
      "proto=tcp"                 # use TCP transport
      "soft"                      # give up after retries (don’t hang forever)
      "intr"                      # allow signals (Ctrl-C) to interrupt
      "timeo=30"                  # initial timeout = 3 s (30 deciseconds)
      "retrans=3"                 # retry only 3 times (~9 s total)
      "_netdev"                   # wait for network before mounting
      # "x-systemd.automount"       # lazy-mount on first access
      # "x-systemd.idle-timeout=60" # auto-unmount after 60 s idle
    ];
  };

  services.paperless = {
    enable = true;
    consumptionDir =  "/mnt/paperless-ngx/consume";
    mediaDir = "/mnt/paperless-ngx/media";
    passwordFile = config.sops.secrets.paperless-password.path;
    port = 9000;
    settings = {
      PAPERLESS_CONSUMER_POLLING = 30;
      PAPERLESS_CONVERT_TMPDIR = "/var/tmp/paperless";
      PAPERLESS_SCRATCH_DIR = "/var/tmp/paperless-scratch";
    };
  };

  sops.secrets = {
    paperless-password = {};
  };

  services.nginx.virtualHosts."paperless.service.home.arpa" = {
    locations."/".proxyPass = "http://localhost:9000";
  };
}
