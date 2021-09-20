final: prev: {
  xmonad-solomon = final.haskellPackages.callCabal2nix "xmonad-solomon" (
    final.lib.sourceByRegex ./.
    [
      "xmonad.hs"
      "xmonad-solomon.cabal"
      "LICENSE"
    ]
  ) { };
}
