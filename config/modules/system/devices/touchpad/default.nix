{ ... }:

{
  services.libinput = {
    enable = true;
    touchpad = {
      disableWhileTyping = true;
      naturalScrolling = false;
      tapping = false;
      clickMethod = "clickfinger";
    };
  };
}
