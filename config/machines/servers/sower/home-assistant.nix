{ config, pkgs, ... }:

let
  home-assistant-image = "ghcr.io/home-assistant/home-assistant:stable";
  home-assistant-port = "8123";
in {
  fileSystems."/mnt/home-assistant" = {
    device = "192.168.5.6:/mnt/tank/app-data/home-assistant";
    fsType = "nfs";
  };

  virtualisation.oci-containers.containers.home-assistant = {
    image = home-assistant-image;
    environment.TZ = "America/Los_Angeles";
    ports = ["${home-assistant-port}:${home-assistant-port}"];

    extraOptions = [
      "--pull=always"
      "--network=host"
    ];

    volumes = ["/mnt/home-assistant:/config"];

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
