{ pkgs, ... }:

{
  primary-user.extraGroups = [ "audio" ];

  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
}
