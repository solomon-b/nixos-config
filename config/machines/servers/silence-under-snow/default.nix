# pi-hole vm
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./pi-hole.nix
    ../../../profiles/virtual-machine
    ../../../modules/services/docker
  ];

  networking = {
    hostName = "silence-under-snow";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };
}
