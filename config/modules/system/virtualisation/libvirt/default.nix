{ pkgs, ... }:

{
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  primary-user.extraGroups = [ "libvirtd" ];
  environment.systemPackages = [ pkgs.virt-manager ];
}
