{
  description = "My XMonad Config";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmonad-solomon/xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
    };

    xmonad-contrib = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmonad-solomon/xmonad-contrib;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };

  outputs = { self, nixpkgs, unstable, flake-utils, xmonad, xmonad-contrib }:
    let
      overlay = import ./overlay.nix;
      overlays = [
        overlay
        xmonad.overlay
        xmonad-contrib.overlay
      ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmonad-solomon p.xmonad-contrib ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
          ];
        };
        defaultPackage = pkgs.haskellPackages.xmonad-solomon;
      }) // { inherit overlay overlays; };
}
