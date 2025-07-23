{ config, lib, pkgs, ... }:

{
  services.slskd = {
    enable = true;
    settings.shares.directories = [
      "/mnt/media/Music"
    ];

    settings.directories = {
      downloads = "/mnt/slskd/downloads";
      incomplete = "/mnt/slskd/incomplete";
    };
    domain = "soulseek.service.home.arpa";
    environmentFile = /var/lib/slskd/env;
    # Figure out how to use SOPS secrets:
    # environmentFile = config.sops.secrets.slskd.path;
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 50300 5030 ];
  };

  fileSystems."/mnt/slskd" = {
    device = "192.168.5.6:/mnt/tank/app-data/slskd";
    fsType = "nfs";
    options = [
      "defaults" # → rw,suid,dev,exec,auto,nouser,async
      "vers=4" # force NFSv3
      "proto=tcp" # use TCP transport
      "intr" # allow signals (Ctrl-C) to interrupt
      "timeo=30" # initial timeout = 3 s (30 deciseconds)
      "retrans=3" # retry only 3 times (~9 s total)
      "_netdev" # wait for network before mounting
    ];
  };

  # sops.secrets = {
  #   slskd = { };
  # };
}
