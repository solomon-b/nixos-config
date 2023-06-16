{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [(modulesPath + "/installer/scan/not-detected.nix")];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "tank/root";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/CC59-434A";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "tank/user";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "tank/nix";
      fsType = "zfs";
    };

  fileSystems."/mnt/nas" = {
    device = "server:/nas";
    fsType = "nfs";
    options = [ "x-systemd.automount" "noauto" ];
  };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/0caab2b6-fad0-4974-9a0b-9000b16967fc"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  nix.settings = {
    build-cores = 2;
    max-jobs = lib.mkDefault 4;
  };

  interfaces.enp0s31f6.useDHCP = true;
  interfaces.wlp4s0.useDHCP = true;
}
