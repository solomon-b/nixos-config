{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./disk-config.nix
  ];

  boot.initrd.systemd.enable = true;
  #boot.initrd.luks.devices.CRYPT.crypttabExtraOpts = [ "fido2-device=auto" ];
  boot.initrd.luks.devices.CRYPT = {
    allowDiscards = true;
    fallbackToPassword = true;
  };

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ "dm-snapshot" "vfat" "nls_cp437" "nls_iso8859-1" "usbhid" ];

  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems = {
    "/".neededForBoot = true;
    "/var/log".neededForBoot = true;
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
