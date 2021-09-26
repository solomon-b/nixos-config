{ ... }:

{
  imports = [
    ./hardware.nix
    ./wireguard.nix
    ../../profiles/pc
  ];

  programs.s3fs.enable = true;

  primary-user.name = "solomon";

  networking = {
    hostName = "sower";
    hostId = "960855f8";
  };
}
