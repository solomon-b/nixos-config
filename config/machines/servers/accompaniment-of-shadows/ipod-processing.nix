{ pkgs, ... }:

let
  ipod-tools = pkgs.buildEnv {
    name = "ipod-tools";
    paths = [
      (pkgs.writeShellApplication {
        name = "postprocess-ipod";
        runtimeInputs = [ pkgs.ffmpeg pkgs.iconv ];
        text = builtins.readFile ./postprocess-ipod.sh;
      })

      (pkgs.writeShellApplication {
        name = "batchprocess-ipod";
        runtimeInputs = [ pkgs.ffmpeg pkgs.iconv ];
        text = builtins.readFile ./batchprocess-ipod.sh;
      })
    ];

    ignoreCollisions = true;
    pathsToLink = [ "/bin" ];
  };
in
{
  environment.systemPackages = [
    ipod-tools
  ];
}
