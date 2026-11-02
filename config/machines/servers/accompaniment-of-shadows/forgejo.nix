{ ... }:

{
  fileSystems."/mnt/forgejo" = {
    device = "192.168.5.6:/mnt/tank/app-data/forgejo";
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

  # The NFS dataset must be owned by this uid and gid.
  users.users.forgejo.uid = 2002;
  users.groups.forgejo.gid = 2002;

  # Without this, forgejo-secrets can write to the root disk under the mount
  # point at boot.
  systemd.services.forgejo.unitConfig.RequiresMountsFor = "/mnt/forgejo";
  systemd.services.forgejo-secrets.unitConfig.RequiresMountsFor = "/mnt/forgejo";

  services.forgejo = {
    enable = true;
    stateDir = "/mnt/forgejo";

    database = {
      type = "postgres";
      createDatabase = true;
    };

    dump.enable = true;

    settings = {
      server = {
        DOMAIN = "forgejo.service.home.arpa";
        ROOT_URL = "http://forgejo.service.home.arpa/";
        # Hermes reaches this port over Tailscale. It cannot reach nginx.
        HTTP_ADDR = "0.0.0.0";
        HTTP_PORT = 3300;
        DISABLE_SSH = true;
      };

      service = {
        DISABLE_REGISTRATION = true;
        REQUIRE_SIGNIN_VIEW = true;
      };

      actions.ENABLED = false;
      mirror.DEFAULT_INTERVAL = "10m";
      migrations.ALLOWED_DOMAINS = "github.com";

      # Keep sessions, the search index, and queues off NFS.
      session.PROVIDER = "db";
      indexer = {
        ISSUE_INDEXER_TYPE = "db";
        REPO_INDEXER_ENABLED = false;
      };
      queue.TYPE = "channel";
    };
  };

  services.nginx.virtualHosts."forgejo.service.home.arpa" = {
    locations."/" = {
      proxyPass = "http://localhost:3300";
      proxyWebsockets = true;
    };
  };
}
