let
  sources = import ../nix/sources.nix;
in
self: super:
{
  xmobar =
    self.haskellPackages.callCabal2nix "xmobar" sources.xmobar-solomon { };
}
