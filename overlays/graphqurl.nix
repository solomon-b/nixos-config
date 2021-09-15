let
  sources = import ../nix/sources.nix;
in
self: super:
{
  graphqurl = (self.callPackage sources.graphqurl-nix { })."graphqurl-1.0.1" ;
}
