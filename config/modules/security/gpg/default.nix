{ pkgs, ... }:

{
  programs.gnupg.agent = {
    enable = true;
    #enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };

  services.udev.packages = [ pkgs.yubikey-personalization ];

  services.pcscd.enable = true;

  environment.systemPackages = [
    pkgs.passage
    pkgs.age-plugin-yubikey
    pkgs.rage
    pkgs.yubikey-manager
    pkgs.yubikey-manager-qt
  ];

  environment.variables = {
    PASSAGE_AGE = "rage";
  };
}
