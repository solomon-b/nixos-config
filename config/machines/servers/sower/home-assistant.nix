{ config, lib, mkPin, ... }:

let
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

  virtualisation.oci-containers.containers.home-assistant = mkPin {
    image = "ghcr.io/home-assistant/home-assistant:2026.6.1";
    imageDigest = "sha256:59aa8824955c9db491b75d2eebe42bd68494f80c2ec69ec0d66d9dae37d37514";
    sha256 = "sha256-dAbiV/GlD/Q/59wZKcygYv6o4U8e0HXKWdsTdF5mjt8=";
    environment.TZ = "America/Los_Angeles";
    ports = [ "${home-assistant-port}:${home-assistant-port}" ];

    extraOptions = [
      "--network=host"
      "--cap-add=NET_ADMIN"
      "--cap-add=NET_RAW"
    ];

    volumes = [ "/mnt/home-assistant:/config" ];

    devices = [
      "/dev/ttyUSB0:/dev/ttyUSB0"
    ];

    autoStart = true;
  };

  # dataDir is an NFS export that maps all clients to the server's mosquitto
  # user, uid 1013 and gid 1015. The mapping applies to the request credential,
  # not to what this client sees, so a local uid of 246 does not own those files
  # here. The pre-start runs `install -m 0700` on the ACL file, and a chmod needs
  # local ownership, so it fails with EPERM. These pins make both sides agree.
  # isSystemUser is implied for a uid below 1000. At 1013 it must be explicit,
  # or the users-groups assertion rejects a user that is neither kind.
  users.users.mosquitto.uid = lib.mkForce 1013;
  users.users.mosquitto.isSystemUser = true;
  users.groups.mosquitto.gid = lib.mkForce 1015;

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
      homeassistant.enabled = true;
      permit_join = true;

      serial = {
        port = "/dev/ttyUSB0";
        baudrate = 115200;
        rtscts = false;
        adapter = "ember";
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
