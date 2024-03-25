{ pkgs, ... }:

{
  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };

  services.jellyseerr = {
    enable = true;
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

  hardware.opengl = {
    enable = true;
    extraPackages = [
      # Hardware transcoding.
      pkgs.intel-media-driver # LIBVA_DRIVER_NAME=iHD
      pkgs.libvdpau-va-gl
      pkgs.vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but can work better for some applications)
      pkgs.vaapiVdpau
      # HDR tone mapping.
      pkgs.intel-ocl
      #pkgs.beignet
      pkgs.intel-compute-runtime
      pkgs.ocl-icd
    ];

    driSupport32Bit = true;

    extraPackages32 = [
      # Hardware transcoding.
      pkgs.intel-media-driver # LIBVA_DRIVER_NAME=iHD
      pkgs.libvdpau-va-gl
      pkgs.vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but can work better for some applications)
      pkgs.vaapiVdpau
      # HDR tone mapping.
      pkgs.intel-ocl
      #pkgs.beignet
      pkgs.intel-compute-runtime
      pkgs.ocl-icd
    ];
  };
}
