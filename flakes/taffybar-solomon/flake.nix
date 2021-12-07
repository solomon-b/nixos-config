{
  description = "My Taffybar Config";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    haskell-language-server = {
      url = github:haskell/haskell-language-server/1.4.0-hackage;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    taffybar = {
      url = github:taffybar/taffybar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, haskell-language-server, flake-utils, taffybar }:
    let
      overlay = import ./overlay.nix;
      overlays = taffybar.overlays ++ [
        overlay
        haskell-language-server.overlay
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
