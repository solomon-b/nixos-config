{ ... }:

{
  imports = [
    ./nixos/network-interfaces.nix
    ./nixos/primary-user.nix
    ./nixos/s3fs.nix

    # ./nixos/powerpanel.nix # Ask Connor
    # ./nixos/preLVMTempMount.nix # Is this temp fs related?
    # ./nixos/secure.nix # Is this for disk encryption?
    # ./nixos/sudo-cmds.nix
  ];

  primary-user.home-manager = _: {
    imports = [
      ./home-manager/termonad.nix
      ./home-manager/kmonad.nix
      ./home-manager/zshExtras.nix
    ];
  };
}
