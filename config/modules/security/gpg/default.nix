{ pkgs, ... }:

{
  programs.gnupg.agent = {
    enable = true;
    #enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };

  services.udev.packages = [ pkgs.yubikey-personalization ];

  environment.systemPackages = [
    pkgs.yubikey-manager
    pkgs.yubikey-manager-qt
  ];
}
