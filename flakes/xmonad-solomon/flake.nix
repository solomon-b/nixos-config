{
  description = "My XMonad Config";

  inputs = {
    nixpkgs = {
      url = github:nixos/nixpkgs/nixos-22.11;
    };

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    git-ignore-nix = {
      url = github:hercules-ci/gitignore.nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad = {
      url = github:xmonad/xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };

    xmonad-contrib = {
      url = github:xmonad/xmonad-contrib;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
  };

  outputs = { self, nixpkgs, unstable, flake-utils, xmonad, xmonad-contrib, git-ignore-nix }:
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
      }) // {
        overlays.default = overlay;
        overlays.xmonad = xmonad.overlay;
        overlays.xmonad-contrib = xmonad-contrib.overlay;
      };
}
