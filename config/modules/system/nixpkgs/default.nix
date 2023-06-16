{ config, lib, inputs, ... }:

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

    nixPath = lib.mkForce [
      "unstable=${inputs.unstable}"
      "nixpkgs=${inputs.nixpkgs}"
    ];

    settings.auto-optimise-store = true;

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
      unstable.flake = inputs.unstable;
    };
  };
}
