{ pkgs, modulesPath, lib, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
  ];

  networking = {
    hostName = "gnostic-ascension";
    # generated for this host; ZFS needs a host ID. 
    hostId = "63d9d016";
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

  # services.nginx = {
  #   enable = true;

  #   recommendedGzipSettings = true;
  #   recommendedOptimisation = true;
  #   recommendedProxySettings = true;
  #   virtualHosts."nude-earth.co".root = ./public;
  #   virtualHosts."short-squeeze.info".root = ./public;
  # };


  # networking.firewall.allowedTCPPorts = [ 80 443 ];

}
