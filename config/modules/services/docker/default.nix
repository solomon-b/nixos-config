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

  primary-user.extraGroups = [ "docker" ];
}
