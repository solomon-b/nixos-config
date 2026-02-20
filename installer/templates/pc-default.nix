{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./disk-config.nix

    ../../../profiles/pc
  ];

  networking.hostName = "{{MACHINE_NAME}}";
  networking.hostId = "{{HOST_ID}}";
}