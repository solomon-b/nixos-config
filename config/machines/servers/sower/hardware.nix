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
      device = "/dev/disk/by-uuid/2F36-F75A";
      fsType = "vfat";
    };

    "/mnt/media" = {
      device = "192.168.5.6:/mnt/tank/Media";
      fsType = "nfs";
      options = [
        "defaults"                  # → rw,suid,dev,exec,auto,nouser,async
        "vers=3"                    # force NFSv3
        "proto=tcp"                 # use TCP transport
        "soft"                      # give up after retries (don’t hang forever)
        "intr"                      # allow signals (Ctrl-C) to interrupt
        "timeo=30"                  # initial timeout = 3 s (30 deciseconds)
        "retrans=3"                 # retry only 3 times (~9 s total)
        "_netdev"                   # wait for network before mounting
        # "x-systemd.automount"       # lazy-mount on first access
        # "x-systemd.idle-timeout=60" # auto-unmount after 60 s idle
      ];
    };

    "/mnt/jellyseerr" = {
      device = "192.168.5.6:/mnt/tank/app-data/jellyseerr";
      fsType = "nfs";
      options = [
        "defaults"                  # → rw,suid,dev,exec,auto,nouser,async
        "vers=3"                    # force NFSv3
        "proto=tcp"                 # use TCP transport
        "soft"                      # give up after retries (don’t hang forever)
        "intr"                      # allow signals (Ctrl-C) to interrupt
        "timeo=30"                  # initial timeout = 3 s (30 deciseconds)
        "retrans=3"                 # retry only 3 times (~9 s total)
        "_netdev"                   # wait for network before mounting
        #"x-systemd.automount"       # lazy-mount on first access
        #"x-systemd.idle-timeout=60" # auto-unmount after 60 s idle
      ];
    };
  };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # nix.buildCores = ???
  # nix.maxJobs = lib.mkDefault ???
}
