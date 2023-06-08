{ ... }:

{
  primary-user.home-manager.services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.9;
    backend = "glx";
    fade = false;
    fadeDelta = 5;
    shadow = true;
    shadowOpacity = 0.75;
  };
}
