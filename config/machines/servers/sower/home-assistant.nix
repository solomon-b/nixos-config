{ config, pkgs, ... }:

{
  services.home-assistant = {
    enable = true;
    config = {
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" "::1" ];
      };

      homeassistant = {
        unit_system = "metric";
        time_zone = config.time.timeZone;
        temperature_unit = "C";
        name = "home";
        latitude = 34.225090;
        longitude = -118.377007;
      };
      http.server_port = 8123;
    };
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
