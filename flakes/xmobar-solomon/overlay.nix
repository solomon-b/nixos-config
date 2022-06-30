final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {

      xmobar =
        let xmobar = prev.fetchFromGitHub {
              owner = "jaor";
              repo = "xmobar";
              rev = "dbe9cba77dd23910103aeb667ef07377ceec9464";
              sha256 = "sha256-uWke/S76FiANCiOg+2RhVoGRyH7It32AAEWuUCUpUpM=";
            };
        in final.haskell.lib.dontCheck (hfinal.callCabal2nix "xmobar" xmobar { });

      xmobar-solomon = hfinal.callCabal2nix "xmobar-solomon" (./.) { };
    });
  });
}
