{ pkgs, ... }:

{
  primary-user.extraGroups = [ "audio" ];

  sound.enable = true;

  security.rtkit.enable = true;
  
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
      
  };

  #hardware.pulseaudio = {
  #  enable = true;
  #  package = pkgs.pulseaudioFull;
  #};
}
