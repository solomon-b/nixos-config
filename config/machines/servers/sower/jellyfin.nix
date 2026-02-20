{ pkgs, ... }:

let
  jellyseerData = "/mnt/jellyseerr";
in
{
  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  systemd.services.jellyfin = {
    serviceConfig = {
      RequiresMountsFor = [ "/mnt/media" ];
      After = [ "mnt-media.automount" ];
    };
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
    "jellyfin.local.home.arpa" = {
      locations."/".proxyPass = "http://localhost:8096";
    };

    "jellyfin.service.home.arpa" = {
      locations."/".proxyPass = "http://localhost:8096";
    };

    "jellyseerr.local.home.arpa" = {
      locations."/".proxyPass = "http://localhost:5055";
    };

    "jellyseerr.service.home.arpa" = {
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
      pkgs.intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but can work better for some applications)
      pkgs.libva-vdpau-driver
      pkgs.intel-compute-runtime
      pkgs.ocl-icd
    ];

    enable32Bit = true;

    extraPackages32 = [
      # Hardware transcoding.
      pkgs.intel-media-driver # LIBVA_DRIVER_NAME=iHD
      pkgs.libvdpau-va-gl
      pkgs.intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but can work better for some applications)
      pkgs.libva-vdpau-driver
      pkgs.intel-compute-runtime
      pkgs.ocl-icd
    ];
  };
}
