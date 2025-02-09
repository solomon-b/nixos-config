{ config, lib, inputs, ... }:

{
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

  nixpkgs.config.permittedInsecurePackages = [
    "aspnetcore-runtime-6.0.36"
    "aspnetcore-runtime-wrapped-6.0.36"
    "dotnet-sdk-6.0.428"
    "dotnet-sdk-wrapped-6.0.428"
  ];
}
