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

  fileSystems."/mnt/mosquitto" = {
    device = "192.168.5.6:/mnt/tank/app-data/mosquitto";
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

  fileSystems."/mnt/zigbee2mqtt" = {
    device = "192.168.5.6:/mnt/tank/app-data/zigbee2mqtt";
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

  networking.firewall = {
    enable = true;
    # For Mosquitto:
    allowedTCPPorts = [ 1883 ];
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

    devices = [
      "/dev/ttyUSB0:/dev/ttyUSB0"
    ];

    autoStart = true;
  };

  # https://nixos.wiki/wiki/Mosquitto
  services.mosquitto = {
    enable = true;
    dataDir = "/mnt/mosquitto";
    # TODO: Add auth:
    listeners = [
      {
        acl = [ "pattern readwrite #" ];
        omitPasswordAuth = true;
        settings.allow_anonymous = true;
      }
    ];
  };

  services.zigbee2mqtt = {
    enable = true;
    dataDir = "/mnt/zigbee2mqtt";
    settings = {
      homeassistant = config.services.home-assistant.enable;
      permit_join = true;

      serial = {
        port = "/dev/ttyUSB0";
        baudrate = 115200;
        rtscts = false;
        adapter = "ezsp";  # Add this line!
      };

      mqtt = {
        server = "mqtt://localhost:1883";
      };

      frontend = {
        port = 8080;
        host = "0.0.0.0";
      };
    };
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

  services.nginx.virtualHosts."zigbee2mqtt.service.home.arpa" = {
    extraConfig = "proxy_buffering off;";
    locations."/" = {
      proxyPass = "http://localhost:8080";
      proxyWebsockets = true;
    };
  };
}
