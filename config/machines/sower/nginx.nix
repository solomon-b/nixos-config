{ ... }:

{
  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;

    virtualHosts = {
      "sower" = {
        root = "/srv/www/sower.galaxybrain.zone";
      };
      "nextcloud.sower" = {
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
