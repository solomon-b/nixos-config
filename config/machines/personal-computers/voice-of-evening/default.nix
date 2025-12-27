{ pkgs, lib, ... }:

{
  imports = [
    ./hardware.nix
    ./arduino.nix
    ./kmonad.nix
    #./nfs.nix
    ./zfs.nix

    ../../../profiles/pc
    ../../../modules/services/virtualisation/virtualbox
    ../../../modules/services/x11vnc
  ];

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.fsPackages = [ pkgs.nfs-utils ];
  services.rpcbind.enable = true;
  boot.supportedFilesystems = [ "nfs" ];

  systemd.services.nvidia-control-devices.enable = false;

  environment.systemPackages = [
    pkgs.acpi
    pkgs.elan
    #(pkgs.clementine.override { withIpod = true;})
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

  # x11vnc for remote desktop access
  # Connect via: vncviewer localhost:5900 (through SSH tunnel or Tailscale)
  services.x11vnc = {
    enable = true;
    localhost = true; # Only accessible via localhost - use SSH tunnel or Tailscale
  };
}
