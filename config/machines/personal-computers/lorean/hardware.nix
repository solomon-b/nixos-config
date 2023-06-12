{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [(modulesPath + "/installer/scan/not-detected.nix")];

  boot = {
    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
      kernelModules = [ "dm-snapshot" "vfat" "nls_cp437" "nls_iso8859-1" "usbhid" ];
      luks = {
        yubikeySupport = true;
        devices = {
          crypt = {
            device = "/dev/sda2";
            preLVM = true;
            yubikey = {
              slot = 2;
              twoFactor = false;
              storage = {
                device = "/dev/sda1";
              };
            };
          };
        };
      };
    };
    kernelModules = [ ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=4G" "mode=755" ];
    };

    "/boot" =
      { device = "/dev/disk/by-label/BOOT";
        fsType = "vfat";
      };

    "/nix" =
      { device = "tank/nix";
        fsType = "zfs";
      };

    # Persistent user state.
    "/home" =
      { device = "tank/persist/home";
        fsType = "zfs";
      };

    # Persistent global state.
    "/persist" = {
      device = "tank/persist/root";
      fsType = "zfs";
      neededForBoot = true;
    };

    "/var/log" = {
      device = "tank/persist/systemd-logs";
      fsType = "zfs";
      neededForBoot = true;
    };

    # TODO:
    # "/persist/docker/containers" = {
    #   device = "tank/persist/docker-containers";
    #   fsType = "zfs";
    # };
  };


  swapDevices =
    [{ device = "/dev/mapper/system-swap"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  nix.settings = {
    build-cores = 2;
    max-jobs = lib.mkDefault 4;
  };

  interfaces.enp0s31f6.useDHCP = true;
  interfaces.wlp4s0.useDHCP = true;
}
