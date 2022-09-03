# Nixos Homelab and Dotfiles

## Structure

- [config](https://github.com/solomon-b/nixos-config/tree/main/config): This folder contains all configurations. It is organized into the following subdirectories: 
  - [machines/physical/<name>](https://github.com/solomon-b/nixos-config/tree/main/config/machines/physical): The entrypoints to physical servers and desktop PCs. 
  - [machines/virtual/<name>](https://github.com/solomon-b/nixos-config/tree/main/config/machines/virtual): The entrypoints to virtual servers.
  - [profiles](https://github.com/solomon-b/nixos-config/tree/main/config/profiles): Basic nixos module sets for virtual machines, physical machines, and PCs. These are imported into machine configurations to form a standard baseline for all my systems.
  - [modules](https://github.com/solomon-b/nixos-config/tree/main/config/modules): Nixos configuration modules which constitute the `profiles`. Any machine unique configuration should go in the `machines/` folder for the specific machine in question.
- [modules](https://github.com/solomon-b/nixos-config/tree/main/modules): Custom nixos and home-manager modules I use in my `config`.
- [installer](https://github.com/solomon-b/nixos-config/tree/main/installer): Custom nixos installer iso I use to quickly provision new machines.
## Usage
### Adding A New Machine
Provision a new machine by your usual methods. I use a custom nixos installer cribbed from [wagdav's homelab](https://github.com/wagdav/homelab). The ISO can be built with the following command:

```
$ nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=iso.nix
```

`iso.nix` includes the provided `installer.sh` and `configuration.nix` in `/etc` on the ISO.

Be sure to update the [authorizedKeys](https://github.com/solomon-b/nixos-config/blob/main/installer/configuration.nix#L30-L36) in `configuration.nix` and to create a [primary-user-password](https://github.com/solomon-b/nixos-config/blob/main/installer/iso.nix#L37) file with a hashed password in `installer/` before building an installer `ISO`.

NOTE: If you don't use the installer ISO and wish to deploy with `colmena` then be sure to provide your root user with an [authorized SSH key](https://github.com/solomon-b/nixos-config/blob/main/installer/configuration.nix#L48).

Once you have a machine provisioned, create a machine configuration in either `config/machines/virtual` or `config/machines/physical`. Be sure to use a `profile` or else the `primary-user` module will not work.

Lastly, your machines must be explicitly listed in [flake.nix](https://github.com/solomon-b/nixos-config/blob/main/flake.nix#L86). If you are deploying with `colmena` then you will either need a DNS entry for your machine names or you will need to set the IP with [deployment.targetHost](https://colmena.cli.rs/unstable/reference/deployment.html#deploymenttargethost).

### Deployment
Desktop machines can be deployed with `nixos-rebuild`:

```
$ sudo nixos-rebuild switch --flake '.#nightshade'
```

Webservers are deployed with [colmena](https://colmena.cli.rs/unstable/reference/deployment.html#deploymenttargethost):

```
$ colmena apply
```

## Prior Art/Inspirations

- https://github.com/cprussin/dotfiles
- https://github.com/jkachmar/dotnix
- https://github.com/wagdav/homelab
