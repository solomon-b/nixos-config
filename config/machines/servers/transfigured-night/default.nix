# Redis Service
{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware.nix
    ../../../profiles/virtual-machine
  ];

  networking.hostName = "transfigured-night";

  services.redis.servers = {
    immich = {
      enable = true;
      openFirewall = true;
      port = 6379;
      requirePassFile = config.sops.secrets.redis-immich-password.path;
      bind = null;
    };

    tubearchivist = {
      enable = true;
      openFirewall = true;
      port = 6380;
      requirePassFile = config.sops.secrets.redis-tubearchivist-password.path;
      bind = null;
    };
  };

  networking.firewall.allowedTCPPorts = [ 6379 ];

  sops.secrets = {
    redis-immich-password = { };
    redis-tubearchivist-password = { };
  };
}
