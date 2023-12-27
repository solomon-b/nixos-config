{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.nix-direnv
  ];
  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
  };

  nixpkgs.overlays = [
    (self: super: { nix-direnv = super.nix-direnv.override { }; })
  ];
}
