{ ... }:

{
  services.libinput = {
    enable = true;
    touchpad = {
      disableWhileTyping = true;
      tapping = false;
      clickMethod = "clickfinger";
    };
  };
}
