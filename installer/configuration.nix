# Default system config for fresh machines.
{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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

  users = {
    mutableUsers = false;
    users.solomon = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "keys"];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHedhPWMgsGFQS7niiFlgkCty/0yS68tVP0pm4x4PQLp"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILVTeNwDsHZX06k+o+fz1wmI8h3q2ks+5C7Mv5ADXo+o"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKOqtwXuYBtWmBM6oKc2EYLsyR0Dl4UM7sFAx6PqitSK"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKK3kMxgeA9ivLG/A81PNKhRJx32r7dzFnl+SZNhBc9K"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMngowB0i16wwKP0j+L207+dr512n+rbfYsh/MAPD+PS solomon@apollyon"
      ];
      passwordFile = "/secrets/primary-user-password";
    };
    users.root.hashedPassword = "*";
  }; 

  nix.trustedUsers = [ "root" "solomon" ];
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  users.users.root.openssh.authorizedKeys.keyFiles = [ /etc/ssh/authorized_keys.d/root ];

  networking = {
    hostName = "nixos";
    interfaces.enp0s4.useDHCP = true;
    #interfaces.enp1s0.useDHCP = true;
    useDHCP = false;
  };

  environment.systemPackages = [ pkgs.vim ];

  system.stateVersion = "22.05";
}
