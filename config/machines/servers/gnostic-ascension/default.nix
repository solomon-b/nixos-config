{ pkgs, modulesPath, lib, ... }:

{
  imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
    ../../../profiles/virtual-machine

    (modulesPath + "/virtualisation/digital-ocean-config.nix")
  ];

  networking.hostName = "gnostic-ascension";

  boot.loader.systemd-boot.enable = false;

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    virtualHosts."nude-earth.co".root = ./public;
    virtualHosts."short-squeeze.info".root = ./public;
  };
  
  networking.firewall.allowedTCPPorts = [ 80 443 ];

}
