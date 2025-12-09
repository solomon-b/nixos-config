# Raspberry Pi Server
{ pkgs, inputs, ... }:

{
  imports = [
    ./hardware.nix
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
    ../../../profiles/virtual-machine
  ];

  networking = {
    hostName = "void-warren";
    # use 'systemd-networkd' to manage networking.
    useNetworkd = true;
    # use 'systemd-resolved' for name resolution.
    dhcpcd.enable = false;
    useDHCP = true;
    firewall.enable = true;
  };

  services.resolved = {
    enable = true;
    fallbackDns = [ "9.9.9.9" "8.8.8.8" ];
  };

  # Enable hardware-specific features for Raspberry Pi
  hardware = {
    raspberry-pi."4".apply-overlays-dtmerge.enable = true;
    deviceTree.enable = true;
  };

  # Optimize for ARM architecture
  nixpkgs.hostPlatform = "aarch64-linux";

  # Enable serial console for debugging
  boot.kernelParams = [ "console=ttyS0,115200n8" "console=tty0" ];
}
