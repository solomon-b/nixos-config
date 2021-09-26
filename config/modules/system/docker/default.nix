{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.docker-compose ];
  virtualisation.docker.enable = true;
  primary-user.extraGroups = [ "docker" ];
}
