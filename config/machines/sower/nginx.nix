{ ... }:

{
  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
  };

  # 8080 == heimdall
  # 8081 == qBitTorrent
  # 8082 == FreshRSS
  # 8083 == Hoogle
  # 8096 == Jellyfin
  # 2342 == Photoprism

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx.virtualHosts."nas.sower" = {
    locations."/".proxyPass = "http://192.168.1.174";
  };
}
