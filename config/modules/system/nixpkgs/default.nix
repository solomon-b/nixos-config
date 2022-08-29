{ config, lib, ... }:

{
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
      dates = "03:15";
    };

    autoOptimiseStore = true;

    registry = {
      unstable = {
        from = {
          type = "indirect";
          id = "unstable";
        };
        to = {
          type = "github";
          owner = "NixOS";
          repo = "nixpkgs";
        };
      };
      flake-utils = {
        from = {
          type = "indirect";
          id = "flake-utils";
        };
        to = {
          type = "github";
          owner = "numtide";
          repo = "flake-utils";
        };
      };
    };
  };
}
