{ pkgs, ... }:

{
  virtualisation = {
    containers = {
      enable = true;
    };

    docker = {
      enable = true;
    };
    oci-containers.backend = "docker";
  };

  environment.systemPackages = [ pkgs.docker-compose ];
  primary-user.extraGroups = [ "docker" ];
}
