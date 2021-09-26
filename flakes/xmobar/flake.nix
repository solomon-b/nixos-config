{
  description = "My XMobar Wrapper";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      xmobar-solomon = pkgs.haskellPackages.callCabal2nix "xmobar-solomon" (./.) { };
    in {
    defaultPackage.x86_64-linux = xmobar-solomon;

    overlay = final: prev: {
      xmobar-solomon = xmobar-solomon;
    };

    devShell.x86_64-linux = import ./shell.nix { inherit pkgs; };
  };
}
