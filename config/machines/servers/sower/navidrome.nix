{ ... }:

{
  services.navidrome = {
    enable = true;
    settings =
      {
        Address = "127.0.0.1";
        Port = 4533;
        MusicFolder = "/mnt/media/Music";
      };
  };

  services.nginx.virtualHosts."navidrome.service.home.arpa" = {
    locations."/".proxyPass = "http://localhost:4533";
  };
}
