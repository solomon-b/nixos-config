{ config, pkgs, ... }:

{
  virtualisation.oci-containers.containers.filebrowser = {
    image = "filebrowser/filebrowser:v2.25.0";
    imageFile = pkgs.dockerTools.pullImage {
      imageName = "filebrowser/filebrowser";
      imageDigest = "sha256:b06f6146754a44a492334947b0caa9ad93509ca1b04da8dacd3a76cdd4a2f471";
      sha256 = "sha256-sHfgPSlm2cdxSJ+euBqRgcGVqxMu0xauIM5yuYmcrBg=";
      finalImageTag = "v2.25.0";
    };
    ports = [ "8081:80" ];
    volumes = [
      "/mnt/storage:/srv"
      "/srv/filebrowser:/database"
    ];
  };

  fileSystems."/mnt/storage" = {
    device = "192.168.5.6:/mnt/tank/solomon";
    fsType = "nfs";
    options = [
      "defaults" # → rw,suid,dev,exec,auto,nouser,async
      "vers=3" # force NFSv3
      "proto=tcp" # use TCP transport
      "intr" # allow signals (Ctrl-C) to interrupt
      "timeo=30" # initial timeout = 3 s (30 deciseconds)
      "retrans=3" # retry only 3 times (~9 s total)
      "_netdev" # wait for network before mounting
    ];
  };

  services.nginx.virtualHosts."filebrowser.service" = {
    locations."/".proxyPass = "http://localhost:8081";
  };
}
