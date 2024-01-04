{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems = {
    "/" = {
      device = "tank/root";
      fsType = "zfs";
    };

    "/home" = {
      device = "tank/user";
      fsType = "zfs";
    };

    "/nix" = {
      device = "tank/nix";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/37BC-35DA";
      fsType = "vfat";
    };

    "/srv/NAS" = {
      device = "rpool/data";
      fsType = "zfs";
    };

    "/mnt/media" = {
      device = "192.168.5.6:/mnt/tank/Media ";
      fsType = "nfs";
    };

    "/mnt/media-ro" = {
      device = "192.168.5.6:/mnt/tank/Media ";
      fsType = "nfs";
      options = [ "ro" ];
    };

    "/mnt/storage" = {
      device = "192.168.5.6:/mnt/tank/solomon";
      fsType = "nfs";
    };
  };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # nix.buildCores = ???
  # nix.maxJobs = lib.mkDefault ???
}
