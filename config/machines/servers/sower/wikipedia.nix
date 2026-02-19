{ pkgs, ... }:

let
  kiwixPort = 8081;
in
{
  fileSystems."/mnt/kiwix" = {
    device = "192.168.5.6:/mnt/tank/app-data/kiwix";
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

  systemd.services.kiwix-serve = {
    description = "Kiwix Server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.kiwix-tools}/bin/kiwix-serve --port ${toString kiwixPort} /mnt/kiwix/*.zim";
      DynamicUser = true;
      ReadOnlyPaths = [ "/mnt/kiwix" ];
      Restart = "on-failure";
    };
  };

  services.nginx.virtualHosts."wikipedia.service.home.arpa" = {
    locations."/".proxyPass = "http://localhost:${toString kiwixPort}";
  };
}
