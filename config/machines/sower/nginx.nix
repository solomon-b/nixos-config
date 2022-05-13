{ config, ... }:

let
  inherit (config.networking) domain hostName;
  fqdn = "${hostName}.${domain}";
in
{
  imports = [
    ../../modules/security/acme
  ];

  users.users.nginx.extraGroups = [ "acme" ];

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx = {
      enable = true;

      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;

      virtualHosts = {
        "qbittorrent.${fqdn}" =  {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
            extraConfig =
              # required when the target is also TLS server with multiple hosts
              "proxy_ssl_server_name on;"
              ;
          };
        };

        "syncthing.${fqdn}" = {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8384";
            extraConfig =
              # required when the target is also TLS server with multiple hosts
              "proxy_ssl_server_name on;"
              ;
          };
        };

        "jellyfin.${fqdn}" =  {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8096";
            extraConfig =
              # required when the target is also TLS server with multiple hosts
              "proxy_ssl_server_name on;"
              ;
          };
        };

        "${fqdn}" = {
          root = "/srv/www/sower.galaxybrain.zone";
        };
      };
  };

  security.acme.certs = {
    "galaxybrain.zone" = {
      credentialsFile = "/secrets/cloudflare-api-token";
      dnsProvider = "cloudflare";
      extraDomainNames = [
        "*.galaxybrain.zone"
        "*.sower.galaxybrain.zone"
      ];

      # Use Cloudflare's DNS resolver rather than the system-provided one to
      # ensure that everything propagates as quickly as possible.
      extraLegoFlags = [ "--dns.resolvers=1.1.1.1:53" ];
    };
  };

}
