{ lib, ... }:

{
  boot.loader = {
    timeout = lib.mkDefault 1;
    systemd-boot = {
      enable = lib.mkDefault true;
      editor = false;
    };
    efi.canTouchEfiVariables = true;
  };
}
