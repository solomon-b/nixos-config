{
  description = "My Taffybar Config";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    taffybar = {
      url = github:taffybar/taffybar;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, taffybar }:
    let
      overlay = import ./overlay.nix;
      overlays = taffybar.overlays ++ [
        overlay
      ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.taffybar-solomon p.taffybar ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
          ];
        };
        defaultPackage = pkgs.haskellPackages.taffybar-solomon;
      }) // { inherit overlay overlays; };
}
