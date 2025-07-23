{ pkgs, ... }:

let
  codexRepo = pkgs.fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = "8f1ea7fa855cc2a130824a272a35dcd031ff519b";
    sha256 = "sha256-gHk2ydublBWuHqli11Dj5C9en2HtwTtwxhuayVHbcXs=";
  };

  codex-cli = pkgs.stdenv.mkDerivation (finalAttrs: {
    pname = "codex-cli";
    version = "0.1.0";
    src = codexRepo;
    sourceRoot = finalAttrs.src.name;
    nativeBuildInputs = with pkgs; [
      nodejs
      pnpm
      pnpm.configHook
      makeWrapper
    ];

    pnpmDeps = pkgs.pnpm.fetchDeps {
      inherit (finalAttrs) pname version src;
      hash = "sha256-cCl+T9IySBj3TDpAcg8jYCOSHTu5vh5rMU5LOCqX8G8=";
      fetcherVersion = 9;
    };

    buildPhase = ''
      cd codex-cli
      pnpm install --frozen-lockfile
      pnpm run build
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/bin
      mkdir -p $out/lib

      install -Dm644 dist/cli.js $out/lib/cli.js
      makeWrapper ${pkgs.nodejs}/bin/node $out/bin/codex \
        --add-flags "$out/lib/cli.js"

      runHook postInstall
    '';
  });
in
{
  environment.systemPackages = [
    codex-cli
  ];
}
