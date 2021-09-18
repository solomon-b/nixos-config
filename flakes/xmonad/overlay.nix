_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: rec {
      xmonad-solomon = self.callCabal2nix "xmonad-solomon" (
        pkgs.lib.sourceByRegex ./.
        [
          "xmonad.hs"
          "xmonad-solomon.cabal"
          "LICENSE"
        ]
      ) { };
    });
  });
}
