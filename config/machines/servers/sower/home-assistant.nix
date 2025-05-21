{ config, pkgs, ... }:

let
  home-assistant-image = "ghcr.io/home-assistant/home-assistant:stable";
  home-assistant-port = "8123";
in
{
  fileSystems."/mnt/home-assistant" = {
    device = "192.168.5.6:/mnt/tank/app-data/home-assistant";
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

  virtualisation.oci-containers.containers.home-assistant = {
    image = home-assistant-image;
    environment.TZ = "America/Los_Angeles";
    ports = [ "${home-assistant-port}:${home-assistant-port}" ];

    extraOptions = [
      "--pull=always"
      "--network=host"
    ];

    volumes = [ "/mnt/home-assistant:/config" ];

    autoStart = true;
  };

  services.nginx.virtualHosts."home-assistant.service.home.arpa" = {
    extraConfig = "proxy_buffering off;";
    locations."/" = {
      proxyPass = "http://localhost:8123";
      proxyWebsockets = true;
    };
  };

  services.nginx.virtualHosts."home-assistant.local.home.arpa" = {
    extraConfig = "proxy_buffering off;";
    locations."/" = {
      proxyPass = "http://localhost:8123";
      proxyWebsockets = true;
    };
  };
}
