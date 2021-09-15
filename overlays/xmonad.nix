let
  sources = import ../nix/sources.nix;
  xmonad = sources.xmonad;
  xmonad-contrib = sources.xmonad-contrib;
  xmonad-extras = sources.xmonad-extras;
in
final: prev:
{
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      xmonad = hfinal.callCabal2nix "xmonad" xmonad { };
      xmonad-contrib = hfinal.callCabal2nix "xmonad-contrib" xmonad-contrib;
      xmonad-extras = hfinal.callCabal2nix "xmonad-extras" xmonad-extras;
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
