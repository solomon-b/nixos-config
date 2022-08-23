final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      xmobar-solomon = hfinal.callCabal2nix "xmobar-solomon" (./.) { };
    });
  });
}
