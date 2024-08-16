{ pkgs, ... }:

let
  jellyseerData = "/mnt/jellyseerr";
in
{
  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  virtualisation.oci-containers.containers = {
    jellyseerr = {
      image = "fallenbagel/jellyseerr:latest";
      ports = [ "5055:5055" ];

      volumes = [
        "${jellyseerData}/config:/app/config"
      ];

      environment = {
        LOG_LEVEL = "debug";
        TZ = "America/Los_Angeles";
      };

      autoStart = true;
    };
  };

  services.nginx.virtualHosts = {
    "jellyfin.local" = {
      locations."/".proxyPass = "http://localhost:8096";
    };

    "jellyfin.service" = {
      locations."/".proxyPass = "http://localhost:8096";
    };

    "jellyseerr.local" = {
      locations."/".proxyPass = "http://localhost:5055";
    };

    "jellyseerr.service" = {
      locations."/".proxyPass = "http://localhost:5055";
    };
  };

  environment.systemPackages = [
    pkgs.jellyfin
    pkgs.jellyfin-web
    pkgs.jellyfin-ffmpeg
  ];

  users.groups.nas.gid = 998;
  users.users.jellyfin.extraGroups = [ "nas" ];

  hardware.graphics = {
    enable = true;
    extraPackages = [
      # Hardware transcoding.
      pkgs.intel-media-driver # LIBVA_DRIVER_NAME=iHD
      pkgs.libvdpau-va-gl
      pkgs.vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but can work better for some applications)
      pkgs.vaapiVdpau
      pkgs.intel-compute-runtime
      pkgs.ocl-icd
    ];

    enable32Bit = true;

    extraPackages32 = [
      # Hardware transcoding.
      pkgs.intel-media-driver # LIBVA_DRIVER_NAME=iHD
      pkgs.libvdpau-va-gl
      pkgs.vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but can work better for some applications)
      pkgs.vaapiVdpau
      pkgs.intel-compute-runtime
      pkgs.ocl-icd
    ];
  };
}
