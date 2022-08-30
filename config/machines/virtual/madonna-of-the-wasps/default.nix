# Tailscale Exit Node and subnet relay
# TODO: write a systemd service to execute:
# sudo tailscale up --accept-routes --advertise-routes=192.168.1.174/32
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix

    ../../../profiles/virtual-machine
  ];

  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  networking = {
    hostName = "madonna-of-the-wasps";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };
}
