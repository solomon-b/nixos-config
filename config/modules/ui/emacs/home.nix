{ config, pkgs, inputs, ... }:

let
  # Doom reads this directory from the Nix store. The ditaa jar path is
  # put in at build time, so it always points to a live store path.
  doomDir = pkgs.runCommand "doom-config" { } ''
    cp -r ${./doom} $out
    chmod -R u+w $out
    substituteInPlace $out/config.el --replace-fail "@ditaa@" "${pkgs.ditaa}"
  '';
in
{
  imports = [ inputs.nix-doom-emacs-unstraightened.homeModule ];

  programs.doom-emacs = {
    enable = true;
    inherit doomDir;
    extraPackages = epkgs: with epkgs; [ vterm ];
  };
}
