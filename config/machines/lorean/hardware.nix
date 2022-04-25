{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "tank/root";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/4F59-6D3A";
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
    [ { device = "/dev/disk/by-uuid/46068d88-87e1-46ce-ac32-e12bab0e1a00"; }
    ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  nix.buildCores = 2;
  nix.maxJobs = lib.mkDefault 4;

  interfaces.enp0s31f6.useDHCP = true;
  interfaces.wlp4s0.useDHCP = true;
}
