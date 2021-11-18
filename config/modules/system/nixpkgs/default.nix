{ ... }:

{
  nixpkgs = {
    config.allowUnfree = true;
  };

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";
  nix.gc.dates = "03:15";
  nix.autoOptimiseStore = true;
}
