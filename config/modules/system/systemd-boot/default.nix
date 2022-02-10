{ ... }:

{
  boot.loader = {
    timeout = 1;
    systemd-boot = {
      enable = true;
      editor = false;
      memtest86.enable = true;
    };
    efi.canTouchEfiVariables = true;
  };
}
