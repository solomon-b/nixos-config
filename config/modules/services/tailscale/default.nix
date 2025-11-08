{ pkgs, ... }:

{
  services.tailscale.enable = true;
  networking.firewall = {
    allowedUDPPorts = [ 41641 ];
    checkReversePath = "loose";
  };
  environment.systemPackages = [ pkgs.tailscale ];
}
