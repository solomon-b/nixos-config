# - [x] Confirm HTTP streaming works.
# - [ ] Setup "satelite" mode; https://wiki.archlinux.org/title/Music_Player_Daemon#Multi-MPD_setup
# = [ ] Find a good android client
{ ... }:

{
  services.mpd = {
    enable = true;
    musicDirectory = "/mnt/media/Music";
    extraConfig = ''
      # must specify one or more outputs in order to play audio!
      # (e.g. ALSA, PulseAudio, PipeWire), see next sections
      audio_output {
      	type		"httpd"
      	name		"My HTTP Stream"
      	encoder		"opus"		      # optional
      	port		"8000"
      	bitrate		"128000"			  
      	format		"48000:16:1"
      	always_on       "yes"			# prevent MPD from disconnecting all listeners when playback is stopped.
      	tags            "yes"			# httpd supports sending tags to listening streams.
      }
    '';
  
    # Optional:
    network.listenAddress = "any";
    startWhenNeeded = true;
  };

  networking.firewall.allowedTCPPorts = [ 6600 8000 ];
}
