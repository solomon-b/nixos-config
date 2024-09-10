{ config, ... }:

{
  fileSystems."/mnt/paperless-ngx" = {
    device = "192.168.5.6:/mnt/tank/app-data/paperless-ngx";
    fsType = "nfs";
  };

  services.paperless = {
    enable = true;
    consumptionDir =  "/mnt/paperless-ngx/consume";
    mediaDir = "/mnt/paperless-ngx/media";
    passwordFile = config.sops.secrets.paperless-password.path;
    port = 9000;
    settings = {
      PAPERLESS_CONSUMER_POLLING = 30;
    };
  };

  sops.secrets = {
    paperless-password = {};
  };

  services.nginx.virtualHosts."paperless.service" = {
    locations."/".proxyPass = "http://localhost:9000";
  };
}
