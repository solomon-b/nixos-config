{
  description = "My XMobar Wrapper";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
    haskell-language-server.url = github:haskell/haskell-language-server;
  };

  outputs = { self, nixpkgs, flake-utils, haskell-language-server }:
    let
      overlay = import ./overlay.nix;
      overlays = [
        overlay
        haskell-language-server.overlay
      ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmobar-solomon ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.pkg-config
          ];
        };
        defaultPackage = pkgs.haskellPackages.xmobar-solomon;
      }
    ) // { inherit overlay overlays; };
}
