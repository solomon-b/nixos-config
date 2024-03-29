{ config, ... }:

{
  virtualisation.oci-containers.containers.filebrowser = {
    image = "filebrowser/filebrowser";
    ports = [ "8081:80" ];
    volumes = [
      "/mnt/storage:/srv"
      "/srv/filebrowser:/database"
    ];
  };

  fileSystems."/mnt/storage" = {
    device = "192.168.5.6:/mnt/tank/solomon";
    fsType = "nfs";
  };

  services.nginx.virtualHosts."filebrowser.service" = {
    locations."/".proxyPass = "http://localhost:8081";
  };
}
