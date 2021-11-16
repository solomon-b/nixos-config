final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      taffybar-solomon = hfinal.callCabal2nix "taffybar-solomon" (./.) { };
        });
  });
}
