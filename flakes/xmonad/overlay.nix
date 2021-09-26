final: prev: {
  xmonad-solomon = final.haskellPackages.callCabal2nix "xmonad-solomon" (
    final.lib.sourceByRegex ./.
    [
      "xmonad.hs"
      "xmonad-solomon.cabal"
      "LICENSE"
    ]
  ) { };

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      #X11 = hfinal.callHackage "X11" "1.10.1" {};
      X11 = hfinal.callPackage
        ({ mkDerivation, base, data-default-class, libX11, libXext
         , libXinerama, libXrandr, libXrender, libXScrnSaver
         }:
         mkDerivation {
           pname = "X11";
           version = "1.10.1";
           sha256 = "13a0qf8rwn1s43wcl39f1pcq3h1kw1ddfq205j1ry0j3yafnazxg";
           libraryHaskellDepends = [ base data-default-class ];
           librarySystemDepends = [
             libX11 libXext libXinerama libXrandr libXrender libXScrnSaver
           ];
           description = "A binding to the X11 graphics library";
           license = prev.lib.licenses.bsd3;
         }) {inherit (prev.xorg) libX11; inherit (prev.xorg) libXScrnSaver;
             inherit (prev.xorg) libXext; inherit (prev.xorg) libXinerama;
             inherit (prev.xorg) libXrandr; inherit (prev.xorg) libXrender;};
        });
  });
}
