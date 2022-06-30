{
  description = "My XMobar Wrapper";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = import ./overlay.nix;
      overlays = [
        overlay
      ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmobar-solomon ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
            pkgs.pkg-config
          ];
        };
        defaultPackage = pkgs.haskellPackages.xmobar-solomon;
      }
    ) // { inherit overlay overlays; };
}
