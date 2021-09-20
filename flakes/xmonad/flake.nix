{
  description = "My XMonad Config";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nixpkgs.url = "nixpkgs/nixos-unstable";
    #xmonad.url = github:xmonad/xmonad;
    xmonad.url = github:IvanMalison/xmonad/nixRecompilationSupport;
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
  };

  outputs = { self, nixpkgs, flake-utils, xmonad, xmonad-contrib }:
    let
      system = "x86_64-linux";
      overlay = import ./overlay.nix;
      overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
      pkgs = import nixpkgs { inherit system overlays; };
      composeOverlays = oa: ob: self: super:
        let super' = super // oa self super;
            super'' = super' // ob self super';
        in super'';
    in {
      overlay = pkgs.lib.foldl composeOverlays overlay overlays;
      defaultPackage.x86_64-linux = pkgs.xmonad-solomon;
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          pkgs.cabal-install
          pkgs.haskellPackages.ghc
          pkgs.haskellPackages.hlint
        ];
      };
    };
}
