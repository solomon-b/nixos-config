{ ... }:
{
  services.hoogle = {
    enable = true;
    port = 8082;

    packages = hp: [
      hp.xmonad
      hp.xmonad-contrib
      hp.lens
      hp.comonad
    ];
  };
}
