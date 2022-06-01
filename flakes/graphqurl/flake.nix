{
  description = "My Graphqurl Wrapper";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      defaultPackage.x86_64-linux = (pkgs.callPackage ./default.nix { })."graphqurl-1.0.1";

      overlay = final: prev: {
        graphqurl = (final.callPackage ./default.nix { })."graphqurl-1.0.1" ;
      };

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          pkgs.nodePackages.node2nix
        ];
      };
    };
}
