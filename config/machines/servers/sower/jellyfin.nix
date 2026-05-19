{ inputs, pkgs, ... }:

{
  # 25.11's nixpkgs still ships the pre-rename jellyseerr 2.7.3 module.
  # Pull unstable's renamed seerr (3.2+) module + package instead.
  disabledModules = [ "services/misc/jellyseerr.nix" ];
  imports = [ "${inputs.unstable}/nixos/modules/services/misc/seerr.nix" ];

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

  services.seerr = {
    enable = true;
    package = inputs.unstable.legacyPackages.${pkgs.system}.seerr;
    port = 5055;
    configDir = "/mnt/jellyseerr/config";
  };

  # The seerr module uses ProtectSystem=strict (no writes outside StateDirectory)
  # and DynamicUser=true (UID changes each restart). Allow writes to the NFS
  # config dir, and ensure the mount is up before the service starts.
  systemd.services.seerr = {
    serviceConfig.ReadWritePaths = [ "/mnt/jellyseerr/config" ];
    unitConfig.RequiresMountsFor = [ "/mnt/jellyseerr" ];
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
