{ pkgs, ... }:

let
  packages = import ./default.nix { inherit pkgs; };
in
{
  home.packages = packages.gui-applications;
}