{
  description = "Xob Volume Bar";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-21.11;

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      defaultPackage.x86_64-linux =
        let watcher = pkgs.callPackage ./derivation.nix { };
        in pkgs.writeShellScriptBin "volume-bar"
          ''
          ${watcher}/bin/pulse-volume-watcher.py | ${pkgs.xob}/bin/xob -c ${./xob.config} -s default
          '';

      overlay = final: prev: {
        volume-bar =
          let watcher = final.callPackage ./derivation.nix { };
          in pkgs.writeShellScriptBin "volume-bar"
            ''
          ${watcher}/bin/pulse-volume-watcher.py | ${pkgs.xob}/bin/xob -c ${./xob.config} -s default
            '';
      };

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          pkgs.python39
          pkgs.python39Packages.pulsectl
          pkgs.xob
        ];
      };
    };
}
