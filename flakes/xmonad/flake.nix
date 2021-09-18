{
  description = "My XMonad Config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs = { self, nixpkgs, xmonad, xmonad-contrib }:
    let
      system = "x86_64-linux";
      overlays = [ xmonad.overlay xmonad-contrib.overlay ];
      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowBroken = true;
      };
      xmonad-solomon = pkgs.haskellPackages.callCabal2nix "xmonad-solomon" (./.) { };
    in {
      defaultPackage.x86_64-linux = xmonad-solomon;

      overlay = final: prev: {
        xmonad-solomon = xmonad-solomon;
      };

      devShell.x86_64-linux = pkgs.haskellPackages.shellFor {
        packages = p: [ p.xmonad-solomon p.xmonad-contrib ];
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          ghc
          #haskell-language-server
          hlint
        ];
      };
    };
}
