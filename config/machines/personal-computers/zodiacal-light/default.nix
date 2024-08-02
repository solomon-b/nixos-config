{ pkgs, ... }:

{
  imports = [
    ./glances.nix
    ./hardware.nix
    ./kmonad.nix
    #./nfs.nix
    ./zfs.nix

    ../../../profiles/pc
    ../../../modules/services/virtualisation/virtualbox
    ../../../modules/system/devices/touchpad
    ../../../modules/system/powertop
  ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.fsPackages = [ pkgs.nfs-utils ];
  services.rpcbind.enable = true;
  boot.supportedFilesystems = [ "nfs" ];

  environment.systemPackages = [
    pkgs.acpi
    pkgs.elan

    # (pkgs.ollama.override { acceleration = "cuda"; })
  ];

  primary-user.name = "solomon";

  sops.secrets.syncoid-ssh-key = {
    owner = "syncoid";
    mode = "600";
  };

  networking = {
    hostName = "zodiacal-light";
    hostId = "02e2867c";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };
}
