{ ... }:

{
  imports = [
    ./hardware.nix
    ./disk-config.nix

    ../../../profiles/virtual-machine
  ];

  networking.hostName = "caleuche";
  networking.hostId = "74baa32f";

  # Only caleuche and Solomon's keys can decrypt this file. caleuche is not a
  # recipient of secrets.yaml.
  sops.defaultSopsFile = ../../../../secrets/hermes.yaml;
}
