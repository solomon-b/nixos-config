{ ... }:

{
  imports = [
    ./hardware.nix
    ./disk-config.nix

    ../../../profiles/virtual-machine
  ];

  networking.hostName = "{{MACHINE_NAME}}";
  networking.hostId = "{{HOST_ID}}";
}
