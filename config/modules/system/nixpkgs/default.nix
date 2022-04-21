{ inputs, config, lib, ... }:

{
  #config = {
  #  permittedInsecurePackages = [
  #    "electron-9.4.4"
  #  ];
  #};

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    nixPath = lib.mkForce [
      "nixpkgs=${inputs.nixpkgs}"
      "unstable=${inputs.unstable}"
    ];

    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
      dates = "03:15";
    };

    autoOptimiseStore = true;
  };
}
