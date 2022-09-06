# Postgres Service
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
  ];

  networking.hostName = "transfigured-night";

  fileSystems."/mnt/postgresql" = {
    device = "192.168.1.174:/mnt/tank/postgresql";
    fsType = "nfs";
  };

  services.postgresql = {
    enable = true;
    dataDir = "/mnt/postgresql";
    enableTCPIP = true;
    package = pkgs.postgresql_14;

    authentication = ''
      host all all nightshade md5
    '';
  };

  networking.firewall.allowedTCPPorts = [ 5432 ];
}
