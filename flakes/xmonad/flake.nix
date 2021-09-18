{
  description = "My XMonad Config";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs = { self, nixpkgs, xmonad, xmonad-contrib }:
    let overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {
            inherit system overlays;
            config.allowBroken = true;
          };
      in
        rec {
          devShell = pkgs.haskellPackages.shelFor {
            packages = p: [ p.xmonad-solomon p.xmonad-contrib ];
            buildInputs = with pkgs.haskellPackages; [
              cabal-install
              #haskell-language-server
              hlint
            ];
          };
          defaultPackage = pkgs.haskellPackages.xmonad-solomon;
        };
    );
}

