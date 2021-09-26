{ pkgs ? import <nixpkgs> { } }:
# let sources = import
#./nix/sources.nix; pkgs = import sources.nixpkgs { };
#
#  easy-hls = pkgs.callPackage sources.easy-hls-nix {
#    ghcVersions = [ "8.10.7" ];
#  };
let
  tooling = [
    pkgs.cabal2nix
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc8107
    pkgs.haskellPackages.xmobar
    pkgs.haskellPackages.font-awesome-type
    #easy-hls
  ];

  dynamicLibraries = [
    pkgs.alsaLib
    pkgs.openssl
    pkgs.pkg-config
    pkgs.xorg.libX11
    pkgs.xorg.libXext
    pkgs.xorg.libXft
    pkgs.xorg.libXpm
    pkgs.xorg.libXrandr
    pkgs.xorg.libXScrnSaver
    pkgs.zlib
    pkgs.zstd
  ];

  includeLibraries = [
    pkgs.openssl.dev
    pkgs.zlib.dev
  ];
in
pkgs.mkShell {
  buildInputs = tooling ++ dynamicLibraries ++ includeLibraries;
  LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath dynamicLibraries;
}
