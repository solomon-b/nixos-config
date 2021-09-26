let
  nix-plugins =
    { lib, stdenv, fetchFromGitHub, nixUnstable, cmake, pkg-config, boost, nlohmann_json }:

    stdenv.mkDerivation rec {
      pname = "nix-plugins";
      version = "d0df32b31f3054180741adf5865fd56d6731c572";

      src = fetchFromGitHub {
        owner = "shlevy";
        repo = "nix-plugins";
        rev = version;
        sha256 = "sha256-Zbc0iq5ZAr73B+NJvpBHm9GIJhb9qrq0vFmV/ucNT5I=";
      };

      nativeBuildInputs = [ cmake pkg-config ];

      buildInputs = [ nixUnstable boost nlohmann_json ];

      meta = {
        description = "Collection of miscellaneous plugins for the nix expression language";
        homepage = "https://github.com/shlevy/nix-plugins";
        license = lib.licenses.mit;
        platforms = lib.platforms.all;
      };
    };
in
final: prev: {
  nix-plugins = final.callPackage nix-plugins { };
}
