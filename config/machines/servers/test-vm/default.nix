{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./disk-config.nix

    ../../../profiles/virtual-machine
  ];

  networking.hostName = "test-vm";
}
