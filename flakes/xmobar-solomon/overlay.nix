final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      xmobar-solomon = hfinal.callCabal2nix "xmobar-solomon" (./.) { };
      xmobar = prev.haskell.lib.overrideCabal hprev.xmobar (old: {
          configureFlags = [
            "-f+with_nl80211"
            "-f+with_alsa"
            "-f+with_xft"
            "-f-with_iwlib"
            "-f+with_threaded"
          ];
          libraryHaskellDepends = old.libraryHaskellDepends ++ [
            hfinal.netlink
          ];
        });
    });
  });
}
