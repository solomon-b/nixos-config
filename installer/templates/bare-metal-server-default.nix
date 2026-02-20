{ ... }:

{
  imports = [
    ./hardware.nix
    ./disk-config.nix

    ../../../profiles/physical-machine
  ];

  networking.hostName = "{{MACHINE_NAME}}";
  networking.hostId = "{{HOST_ID}}";
}
