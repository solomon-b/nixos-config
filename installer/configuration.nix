# Default system config for fresh machines.
{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    extraConfig = "PermitUserEnvironment yes";
  };

  programs.ssh.startAgent = true;

  services.sshd.enable = true;

  security.pam = {
    sshAgentAuth.enable = true;
    services.sudo.sshAgentAuth = true;
  };

  users = {
    mutableUsers = false;
    users.solomon = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "keys" ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHedhPWMgsGFQS7niiFlgkCty/0yS68tVP0pm4x4PQLp solomon@nightshade"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILVTeNwDsHZX06k+o+fz1wmI8h3q2ks+5C7Mv5ADXo+o solomon@lorean"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKK3kMxgeA9ivLG/A81PNKhRJx32r7dzFnl+SZNhBc9K solomon@sower"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMngowB0i16wwKP0j+L207+dr512n+rbfYsh/MAPD+PS solomon@apollyon"
      ];
      passwordFile = "/etc/primary-user-password";
    };
    users.root.hashedPassword = "*";
  };

  nix.settings.trusted-users = [ "root" "solomon" ];
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  users.users.root.openssh.authorizedKeys.keyFiles = [ /etc/ssh/authorized_keys.d/root ];

  networking = {
    hostName = "nixos";
    wireless.enable = true;
    useDHCP = lib.mkDefault true;
    # TODO: This should be generated in the install.sh script
    hostId = "997f3c8d";
  };

  environment.systemPackages = [ pkgs.vim pkgs.git ];

  system.stateVersion = "22.05";
}
