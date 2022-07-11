{ ... }:

{
  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
