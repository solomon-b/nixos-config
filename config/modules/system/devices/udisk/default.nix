{ pkgs, ... }:

{
  services.udisks2 = {
    enable = true;
    mountOnMedia = true;
  };
}
