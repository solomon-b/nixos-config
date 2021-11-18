{ pkgs, lib, config, ... }:
let
  public-keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILVTeNwDsHZX06k+o+fz1wmI8h3q2ks+5C7Mv5ADXo+o lorean"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDnQP3PK7d7y8k05h+zVqXVxSnvs3R5d+PUujqSPXnwZxqNLi4rJ7sWNbT91jQRkZIQ55i9gRMdlv5TcyyP6aXqOtfeTX6Jh5/ysbuzkidszHl4oRDkahr6vMkEcyt91rSjLsjXgHnTAKQLRTaLSOLPddnAdO8cH7cIcxKFUr4y2XqkKPlQm1OPMGNWARReog1/uimZFQbiD4K98yTUNY2vHRxKlubO4gxuSNFMTi/uLeJUwjqRDrU3PEpV3tXebhehgROBzIL69aX9hWSBlzwYbP4kt+Z8duQ0o7+ci4do3dtZmmWn6lTmy+VmVU04xEqg8qzJ1Vm4WjkHjAAY7CjFyZoWpJIBUYCtQ+dnQygwKOAtldlKZMM8b8o/mweEZOMnFhiraXHGOXtJfh7XmVqwAxIAXe8PBq6IT5OMI0eR720NeIepV5Mn68lPTZy3PmTQ9rv5RFFNg7/6M3gJk7cFqNLe/8sBhZ6jxkSVikpHSw/FEMMz3x1qLRhekr7mBL0= solomon@sower"
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
