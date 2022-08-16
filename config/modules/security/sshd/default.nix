{ pkgs, lib, config, ... }:
let
  public-keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHedhPWMgsGFQS7niiFlgkCty/0yS68tVP0pm4x4PQLp"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILVTeNwDsHZX06k+o+fz1wmI8h3q2ks+5C7Mv5ADXo+o"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKOqtwXuYBtWmBM6oKc2EYLsyR0Dl4UM7sFAx6PqitSK"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKK3kMxgeA9ivLG/A81PNKhRJx32r7dzFnl+SZNhBc9K"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMngowB0i16wwKP0j+L207+dr512n+rbfYsh/MAPD+PS solomon@apollyon"
  ];
in
{
  services.openssh = {
    enable = true;
    passwordAuthentication = true;
    permitRootLogin = "no";
    extraConfig = "PermitUserEnvironment yes";
  };

  programs.ssh.startAgent = true;

  services.sshd.enable = true;

  security.pam = {
    enableSSHAgentAuth = true;
    services.sudo.sshAgentAuth = true;
  };

  primary-user.openssh.authorizedKeys.keys = public-keys;
}
