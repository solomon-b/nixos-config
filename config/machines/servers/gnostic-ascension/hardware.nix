{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ];

  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "virtio_scsi" "virtio_blk" ];
  # boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
  # boot.extraModulePackages = [ ];

  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub = {
    efiSupport = true;

    efiInstallAsRemovable = true;
  };

  # fileSystems = {
  #   "/".neededForBoot = true;
  #   "/var/log".neededForBoot = true;
  # };

  # networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
