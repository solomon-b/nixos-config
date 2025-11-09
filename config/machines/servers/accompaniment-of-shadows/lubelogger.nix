{ ... }:

{
  fileSystems."/mnt/lubelogger" = {
    device = "192.168.5.6:/mnt/tank/app-data/lubelogger";
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

  services.lubelogger = {
    enable = true;
    port = 5000;
    dataDir = "lubelogger";
  };

  services.nginx.virtualHosts."lubelogger.service.home.arpa" = {
    locations."/" = {
      proxyPass = "http://localhost:5000";
      proxyWebsockets = true;
    };
  };
}
