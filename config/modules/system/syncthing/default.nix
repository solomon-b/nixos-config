{ ... }:
{
  services.syncthing = {
    enable = true;
    user = "solomon";
    dataDir = "/Public";
    configDir = "/home/solomon/.config/syncthing";
  };
}
