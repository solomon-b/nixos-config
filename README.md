# Nixos Homelab and Dotfiles

## Structure

- [config](https://github.com/solomon-b/nixos-config/tree/main/config): This folder contains all configurations. It is organized into the following subdirectories: 
  - [machines/personal-computers/<name>](https://github.com/solomon-b/nixos-config/tree/main/config/machines/personal-computers): The entrypoints to desktop PCs. 
  - [machines/servers/<name>](https://github.com/solomon-b/nixos-config/tree/main/config/machines/servers): The entrypoints to servers.
  - [profiles](https://github.com/solomon-b/nixos-config/tree/main/config/profiles): Basic nixos module sets for virtual machines, physical machines, and PCs. These are imported into machine configurations to form a standard baseline for all my systems.
  - [modules](https://github.com/solomon-b/nixos-config/tree/main/config/modules): Nixos configuration modules which constitute the `profiles`. Any machine unique configuration should go in the `machines/` folder for the specific machine in question.
- [modules](https://github.com/solomon-b/nixos-config/tree/main/modules): Custom nixos and home-manager modules I use in my `config`.
- [installer](https://github.com/solomon-b/nixos-config/tree/main/installer): Custom nixos installer iso I use to quickly provision new machines.
## Usage
### Custom Installer ISO

Build a custom NixOS installer ISO with your SSH keys pre-authorized for root:
```
nix build '.#nixos-iso'
```
This ISO includes SSH public keys from `config/modules/security/sshd/public-keys.nix`. Keep it on a USB stick for provisioning new machines.

### Adding A New Machine

1. Boot the new machine from the custom ISO.

2. Run the installer script. It will prompt you to select or create a machine, choose machine type (personal computer, bare metal server, or VM), and enter the target IP. It handles everything: directory creation, config scaffolding, hardware detection, SSH host key generation, hostId generation, SOPS registration, and [nixos-anywhere](https://github.com/numtide/nixos-anywhere) installation. For PCs, it also prompts for a LUKS password and generates user SSH keys.
```
nix run '.#install'
```

### Post install
Once you have your machine provisioned you can use `colmena apply --on $MACHINE` to deploy it.

If you are deploying with [colmena](https://colmena.cli.rs/unstable/reference/deployment.html#deploymenttargethost) then you will either need a DNS entry for your machine names or you will need to tweak the `mkMachine` function and set the IP with [deployment.targetHost](https://colmena.cli.rs/unstable/reference/deployment.html#deploymenttargethost).

### Deployment
Build all servers:
```
$ colmena apply --on @server
```

Build a specific server:
```
$ colmena apply --on @sower
```

Build all PCs:
```
$ colmena apply --on @pc
```

## Prior Art/Inspirations

- https://github.com/cprussin/dotfiles
- https://github.com/jkachmar/dotnix
- https://github.com/wagdav/homelab
