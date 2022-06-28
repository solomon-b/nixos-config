{
  description = "podcast-dl";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      nodePackages = import ./default.nix {
        inherit pkgs system;
        nodejs = pkgs."nodejs-14_x";
      };
    in {
      defaultPackage.x86_64-linux = nodePackages."podcast-dl-7.3.2";

      overlay = final: prev: {
        podcast-dl = nodePackages."podcast-dl-7.3.2" ;
      };

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          pkgs.nodePackages.node2nix
        ];
      };
    };
}
