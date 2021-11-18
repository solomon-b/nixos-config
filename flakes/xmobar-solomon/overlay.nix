final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {

      xmobar =
        let xmobar = prev.fetchFromGitHub {
              owner = "jaor";
              repo = "xmobar";
              rev = "397953f1c626a3a81b9ef7280d961fb3ce340c56";
              sha256 = "sha256-O42xW58EJBE7BIrApHF7EXlWoOeosLcn/cCy42aABrA=";
            };
        in final.haskell.lib.dontCheck (hfinal.callCabal2nix "xmobar" xmobar { });

      xmobar-solomon = hfinal.callCabal2nix "xmobar-solomon" (./.) { };
    });
  });
}
