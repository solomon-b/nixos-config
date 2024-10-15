{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./disk-config.nix
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" "vfat" "nls_cp437" "nls_iso8859-1" "usbhid" ];

  boot.initrd.luks.devices.CRYPT = {
    allowDiscards = true;
    fallbackToPassword = true;
  };

  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems = {
    "/".neededForBoot = true;
    "/var/log".neededForBoot = true;
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware.nvidia.open = false;

  zramSwap.enable = true;

  nix.settings = {
    build-cores = 2;
    max-jobs = lib.mkDefault 4;
  };
}
