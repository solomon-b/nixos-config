{ ... }:

{
  programs.gnupg.agent = {
    enable = true;
    #enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };
}
