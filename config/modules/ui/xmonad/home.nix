{ pkgs, ... }:

{
  xsession = {
    enable = true;
    windowManager.command = "xmonad-solomon";
  };
}
