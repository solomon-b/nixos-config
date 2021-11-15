{
  description = "My XMobar Wrapper";

  inputs = {
    #nixpkgs.url = path:../../nixpkgs;
    nixpkgs.url = github:NixOS/nixpkgs/nixos-21.05;
    flake-utils.url = github:numtide/flake-utils;
    easy-hls = {
      url = github:ssbothwell/easy-hls-nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    #haskell-language-server.url = github:haskell/haskell-language-server;
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls }:
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
            pkgs.cabal-install
            easy-hls
            pkgs.pkg-config
          ];
        };
        defaultPackage = pkgs.haskellPackages.xmobar-solomon;
      }
    ) // { inherit overlay overlays; };
}
