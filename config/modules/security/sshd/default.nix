{ pkgs, lib, config, ... }:
{
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = true;
    extraConfig = "PermitUserEnvironment yes";
  };

  programs.ssh.startAgent = true;

  services.sshd.enable = true;

  security.pam = {
    sshAgentAuth.enable = true;
    services.sudo.sshAgentAuth = true;
  };

  primary-user.openssh.authorizedKeys.keys = import ./public-keys.nix;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHedhPWMgsGFQS7niiFlgkCty/0yS68tVP0pm4x4PQLp solomon@nightshade"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILVTeNwDsHZX06k+o+fz1wmI8h3q2ks+5C7Mv5ADXo+o solomon@lorean"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJHWnMYdY2wfu05WThiGKlNK8aCX3HmNyQds8MOoSM+v solomon@voice-of-evening"
  ];
}
