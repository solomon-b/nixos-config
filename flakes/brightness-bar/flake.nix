{
  description = "Xob Brightness Bar";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      defaultPackage.x86_64-linux =
        let watcher = pkgs.callPackage ./derivation.nix { };
        in pkgs.writeShellScriptBin "brightness-bar"
          ''
          ${watcher}/bin/brightness-watcher.py | ${pkgs.xob}/bin/xob -c ${./xob.config} -s default
          '';

      overlay = final: prev: {
        brightness-bar =
          let watcher = final.callPackage ./derivation.nix { };
          in pkgs.writeShellScriptBin "brightness-bar"
            ''
          ${watcher}/bin/brightness-watcher.py | ${pkgs.xob}/bin/xob -c ${./xob.config} -s default
            '';
      };

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          pkgs.python39
          pkgs.python39Packages.watchdog
          pkgs.xob
        ];
      };
    };
}
