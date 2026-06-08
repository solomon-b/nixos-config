{ mkPin, ... }:

let
  homeboxData = "/mnt/homebox";
in
{
  fileSystems."/mnt/homebox" = {
    device = "192.168.5.6:/mnt/tank/app-data/homebox";
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

  virtualisation.oci-containers.containers = {
    homebox = mkPin {
      image = "ghcr.io/hay-kot/homebox:v0.10.3";
      imageDigest = "sha256:b1fbca6fc1ad2bd3634d13056d1bc0242cf92e9bad4ca21ce57589be3fe4987e";
      sha256 = "sha256-/CPkQBmQb/PJgVkhhmSat5waRf/jJHhcIx5Cnz65K0s=";
      ports = [ "3100:7745" ];

      volumes = [
        "${homeboxData}:/data/"
      ];

      environment = {
        HBOX_LOG_LEVEL = "info";
        HBOX_LOG_FORMAT = "text";
        HBOX_WEB_MAX_UPLOAD_SIZE = "10";
      };

      autoStart = true;
    };
  };

  services.nginx.virtualHosts."homebox.service.home.arpa" = {
    locations."/" = {
      proxyPass = "http://localhost:3100";
      proxyWebsockets = true;
    };
  };
}
