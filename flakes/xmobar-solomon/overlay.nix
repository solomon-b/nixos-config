final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {

     xmobar-solomon = hfinal.callCabal2nix "xmobar-solomon" (./.) { };
      #xmobar-solomon = hfinal.callCabal2nix "xmobar-solomon" (
      #  final.lib.sourceByRegex ./.
      #  [
      #    "./app/Main.hs"
      #    "./src/App/Acpi.hs"
      #    "./src/App/Icons.hs"
      #    "./src/App/DunstStatus.hs"
      #    "cabal.project"
      #    "xmobar-solomon.cabal"
      #    "LICENSE"
      #  ]
      #) { };
    });
  });
}
