{ ... }:

{
  services.hoogle = {
    enable = true;
    port = 8083;

    # TODO: enable all packages
    packages = hp: [
      hp.xmonad
      hp.xmonad-contrib
      hp.lens
      hp.comonad
    ];
  };

  services.nginx.virtualHosts."hoogle.sower" = {
    locations."/".proxyPass = "http://localhost:8083";
  };
}
