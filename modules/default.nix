{ ... }:

{
  imports = [
    ./nixos/network-interfaces.nix
    ./nixos/primary-user.nix
    ./nixos/qbittorrent.nix
    ./nixos/s3fs.nix

    # ./nixos/powerpanel.nix # Ask Connor
    # ./nixos/preLVMTempMount.nix # Is this temp fs related?
    # ./nixos/secure.nix # Is this for disk encryption?
    # ./nixos/sudo-cmds.nix
  ];
}
