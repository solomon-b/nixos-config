# https://nixos.wiki/wiki/MPD#PipeWire
{ pkgs, ... }:

{
  services.mpd = {
    enable = true;
    user = "solomon";
    musicDirectory = "/mnt/media/Music";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "System PulseAudio" # this can be whatever you want
      }
    '';
    startWhenNeeded = true;
  };

  systemd.services.mpd.environment = {
      # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
      XDG_RUNTIME_DIR = "/run/user/1000"; # User-id 1000 must match above user. MPD will look inside this directory for the PipeWire socket.
  };

  primary-user.home-manager.programs.ncmpcpp = {
    enable = true;
  };
}
