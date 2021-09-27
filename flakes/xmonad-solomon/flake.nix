{
  description = "My XMonad Config";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nixpkgs.url = "nixpkgs/nixos-unstable";
    haskell-language-server.url = github:haskell/haskell-language-server;

    xmonad = {
      url = github:ssbothwell/xmonad;
      #url = path:./xmonad;
      #url = github:IvanMalison/xmonad/nixRecompilationSupport;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-contrib = {
      url = github:ssbothwell/xmonad-contrib;
      #url = path:./xmonad-contrib;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskell-language-server, xmonad, xmonad-contrib }:
    let
      overlay = import ./overlay.nix;
      overlays = [
        overlay
        xmonad.overlay
        xmonad-contrib.overlay
        haskell-language-server.overlay
      ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmonad-solomon p.xmonad-contrib ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
          ];
        };
        defaultPackage = pkgs.haskellPackages.xmonad-solomon;
      }) // { inherit overlay overlays; };
}
