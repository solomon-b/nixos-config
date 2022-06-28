{ ... }:

{
  services.podgrab = {
    enable = true;
    port = 8081;
    exposePort = true;
    dataDir = "/srv/NAS/Media/Podcasts";
  };
}
