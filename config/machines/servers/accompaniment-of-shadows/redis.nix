# Redis Service
{ config, pkgs, lib, ... }:

{
  # Redis for Immich
  services.redis.servers.immich = {
    enable = true;
    openFirewall = true;
    port = 6379;
    requirePassFile = config.sops.secrets.redis-immich-password.path;
    bind = null; # Allow connections from all interfaces
  };

  networking.firewall.allowedTCPPorts = [ 6379 ];

  sops.secrets.redis-immich-password = { };
}
