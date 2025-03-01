{ pkgs, lib, ... }:

{
  imports = [
    ./hardware.nix
    ./kmonad.nix
    #./nfs.nix
    ./zfs.nix

    ../../../profiles/pc
    ../../../modules/services/virtualisation/virtualbox
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
    (pkgs.clementine.override { withIpod = true;})
    # (pkgs.ollama.override { acceleration = "cuda"; })
  ];

  services.xserver = {
    videoDrivers = lib.mkForce [
      "modesetting"
      "fbdev"
    ];
  };

  primary-user.name = "solomon";

  sops.secrets.syncoid-ssh-key = {
    owner = "syncoid";
    mode = "600";
  };

  networking = {
    hostName = "voice-of-evening";
    hostId = "5f111f12";
    networkmanager.enable = true;

    useDHCP = false;
    interfaces.wlp4s0.useDHCP = true;
  };
}
