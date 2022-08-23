{
  description = "My XMobar Wrapper";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmobar = {
      url = "/home/solomon/Development/haskell/xmobar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, xmobar }:
    let
      overlay = import ./overlay.nix;
      overlays = [
        xmobar.overlay
        overlay
      ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
          dynamicLibraries = with pkgs; [
            xorg.libX11
            xorg.libXrandr
            xorg.libXrender
            xorg.libXScrnSaver
            xorg.libXext
            xorg.libXft
            xorg.libXpm.out
            xorg.libXrandr
            xorg.libXrender
          ];
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmobar-solomon ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
            pkgs.pkg-config
          ] ++ dynamicLibraries;
        };
        defaultPackage = pkgs.haskellPackages.xmobar-solomon;
      }
    ) // { inherit overlay overlays; };
}
