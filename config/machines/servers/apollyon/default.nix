# qBittorrent
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./wireguard.nix

    ../../../profiles/virtual-machine
  ];

  networking.hostName = "apollyon";

  services.qBittorrent = {
    enable = true;
    openFirewall = true;
    webUIAddress.port = 8081;
  };

  fileSystems."/mnt/media" = {
    device = "192.168.5.6:/mnt/tank/Media";
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
}
