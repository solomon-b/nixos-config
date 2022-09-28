# https://github.com/jeslie0/fonts
{
  description =
    "A flake giving access to fonts that I use, outside of nixpkgs.";

  inputs.flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.feather = pkgs.stdenvNoCC.mkDerivation {
          name = "feather-font";
          dontConfigue = true;
          dontUnpack = true;
          src = ./feather.ttf;
          installPhase = ''
            install -D $src -t $out/share/fonts/truetype/
          '';
          meta = { description = "The Feather Font."; };
        };

        packages.default = packages.feather;
      }) // {
        overlays.default = final: prev: {
          feather = self.packages.${final.system}.default;
        };
      };
}
