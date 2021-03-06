{
  description = "My Graphqurl Wrapper";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      nodePackages = import ./default.nix {
        inherit pkgs system;
        nodejs = pkgs."nodejs-14_x";
      };
    in {
      defaultPackage.x86_64-linux = nodePackages."graphqurl-1.0.1";

      overlay = final: prev: {
        graphqurl = nodePackages."graphqurl-1.0.1" ;
      };

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          pkgs.nodePackages.node2nix
        ];
      };
    };
}
