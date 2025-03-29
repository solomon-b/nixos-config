{ pkgs, ... }:

{
  primary-user = {
    home-manager.imports = [./home.nix];
  };
}
