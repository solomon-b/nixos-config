# This is a virtual machine running on the TrueNas Server 'Sandra-Voi'
# which spawns a pi-hole instance.
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./pi-hole.nix
    ../../../profiles/virtual-machine
    ../../../modules/services/docker
  ];

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  primary-user.name = "solomon";

  networking = {
    hostName = "silence-under-snow";
    interfaces.enp0s4.useDHCP = true;
    useDHCP = false;
  };

  system.stateVersion = "22.05";
}
