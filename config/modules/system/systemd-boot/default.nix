{ ... }:

{
  boot.loader = {
    timeout = 10;
    systemd-boot = {
      enable = true;
      editor = false;
    };
    efi.canTouchEfiVariables = true;
  };
}
