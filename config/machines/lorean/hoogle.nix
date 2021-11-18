{ ... }:
{
  services.hoogle = {
    enable = true;
    port = 8081;

    packages = hp: [
      hp.xmonad
      hp.xmonad-contrib
      hp.lens
      hp.comonad
    ];
  };
}
