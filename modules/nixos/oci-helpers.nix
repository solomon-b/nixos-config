{ pkgs, lib, ... }:

{
  # Build an oci-containers.containers.<name> attrset from a single image
  # reference plus pin data. Keeps `image`, `imageFile`, and the embedded
  # `finalImageTag` in lockstep so they can't drift apart — only one image
  # string is written.
  #
  # Any extra attrs (ports, volumes, environment, ...) are passed through
  # unchanged, so the call site reads as a normal container definition.
  _module.args.mkPin = { image, imageDigest, sha256, ... }@args:
    let
      parts = lib.splitString ":" image;
      tag = lib.last parts;
      name = lib.concatStringsSep ":" (lib.init parts);
    in
    (builtins.removeAttrs args [ "imageDigest" "sha256" ]) // {
      imageFile = pkgs.dockerTools.pullImage {
        imageName = name;
        inherit imageDigest sha256;
        finalImageTag = tag;
      };
    };
}
