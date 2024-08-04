{ config, pkgs, ... }:

let
  homeAssistantPort = "8123";
in {
  virtualisation.oci-containers.containers.home-assistant = {
    image = "ghcr.io/home-assistant/home-assistant:stable";
    environment.TZ = "America/Los_Angeles";
    ports = ["127.0.0.1:${homeAssistantPort}:${homeAssistantPort}"];
    extraOptions =
      [
        "--device=/dev/ttyACM0:/dev/ttyACM0"
      ]
      ++ (
        lib.mapAttrsToList
        (host: node: "--add-host=${host}:${node.address}")
        (builtins.removeAttrs network.home ["id" "prefixLength" "router"])
      );
    volumes = ["/var/lib/hass:/config"];

    autoStart = true;
  };


  services.nginx.virtualHosts."home-assistant.service" = {
    locations."/" = {
      proxyPass = "http://localhost:8123";
    };
  };

  services.nginx.virtualHosts."home-assistant.local" = {
    locations."/" = {
      proxyPass = "http://localhost:8123";
    };
  };
}
