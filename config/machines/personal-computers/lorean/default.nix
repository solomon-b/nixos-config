{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./immich-sdcard-sync.nix
    ./kmonad.nix
    ./zfs.nix

    ../../../profiles/pc
    #../../../modules/services/virtualisation/libvirt
    ../../../modules/services/virtualisation/virtualbox
    ../../../modules/system/devices/touchpad
    ../../../modules/system/powertop
  ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  environment.systemPackages = [
    pkgs.acpi
    (pkgs.agda.withPackages (p: [ p._1lab p.standard-library ]))
    pkgs.nfs-utils
  ];

  primary-user.name = "solomon";

  sops.secrets.syncoid-ssh-key = {
    owner = "syncoid";
    mode = "600";
  };

  systemd.services.nvidia-control-devices.enable = false;

  # "...run the OOM killer earlier, but paradoxically that can help clean up wasteful resources and avoid a situation where you end up swamped at the limit" --jkachmar
  services.earlyoom.enable = true;

  services.printing.enable = true;

  services.rpcbind.enable = true;

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  networking = {
    hostName = "lorean";
    hostId = "960855f8";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp4s0.useDHCP = true;
  };
}
