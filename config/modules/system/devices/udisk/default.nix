{ pkgs, ... }:

{
  services.udisks2 = {
    enable = true;
    mountOnMedia = true;
  };

  services.dbus.enable = true;

  primary-user.home-manager = {
    services.udiskie = {
      enable = true;
    };

    # https://github.com/nix-community/home-manager/issues/2064
	  systemd.user.targets.tray = {
	  	Unit = {
	  		Description = "Home Manager System Tray";
	  		Requires = [ "graphical-session-pre.target" ];
	  	};
    };
	};
}
