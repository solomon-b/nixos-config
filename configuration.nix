{ pkgs, config, ... }:

{
  environment.systemPackages = [ 
    pkgs.vim
  ];

  networking = {
    hostName = "test-vm";
  };

  users = {
    users.solomon = {
      password = "password";
      group = "solomon";
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
    mutableUsers = false;

    motd = with config; ''
      Welcome to ${networking.hostName}

      - This machine is managed by NixOS
      - All changes are futile

      OS:      NixOS ${system.nixos.release} (${system.nixos.codeName})
      Version: ${system.nixos.version}
      Kernel:  ${boot.kernelPackages.kernel.version}
    '';
  };
}
