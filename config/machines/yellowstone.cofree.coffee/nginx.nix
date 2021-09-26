{ config, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    # Only allow PFS-enabled ciphers with AES256
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";

    # Setup Nextcloud virtual host to listen on ports
    virtualHosts = {
      "yellowstone.cofree.coffee" = {
        ## Force HTTP redirect to HTTPS
        forceSSL = true;
        ## LetsEncrypt
        enableACME = true;
      };

      "test.cofree.coffee" = {
        forceSSL = true;
        enableACME = true;
        root = "/var/www/cofree.coffee";
      };

      "myconotes.cofree.coffee" = {
        forceSSL = true;
        enableACME = true;
        root = "/var/www/myconotes";
      };
    };
  };

  systemd.services."cofree.coffee" = {
    description = "Fetch the latest version of the blog from github";
    serviceConfig = {
      Type = "oneshot";
    };
    startAt = "*:0/5";
    environment.NIX_PATH = "nixpkgs=/nix/store/9vgwmgpm63ixca62gqrp6mrmwydcy0yk-nixpkgs-src";
    path = with pkgs; [ nix gnutar gzip curl jq ];
    script = ''
      set -ex

      rev=$(curl https://api.github.com/repos/ssbothwell/cofree.coffee/git/ref/heads/main | jq -r .object.sha)
      result=$(nix-build https://github.com/ssbothwell/cofree.coffee/archive/$rev.tar.gz)

      ln -sfT $result /var/www/cofree.coffee
    '';
  };
}
