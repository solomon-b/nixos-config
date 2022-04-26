{ pkgs, ... }:

{
  imports = [
    ../base

    ../../modules/system/systemd-boot
    ../../modules/system/timezone
  ];

  environment.systemPackages = with pkgs; [
    inetutils
    pass
  ];
}
