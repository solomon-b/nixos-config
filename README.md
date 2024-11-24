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
### Adding A New Machine

1. Create a machine profile in `config/machines`. Import a non-existent `./hardware.nix` file.
2. Build a fresh boot disk:
```
nix build '.#nixos-iso'
```
3. Boot the new machine off the iso.
4. Generate a hardware config for the new machine:
```
ssh "root@${IP}" nixos-generate-config --no-filesystems --show-hardware-config > "config/machines/servers/${MACHINE}/hardware.nix"
```
5. Add ssh host keys to `pass`:
```
ssh "root@${IP}" cat /etc/ssh/ssh_host_ed25519_key.pub | pass insert -m "machine/${MACHINE}/ssh-host-key/ed25519/public"
ssh "root@${IP}" cat /etc/ssh/ssh_host_ed25519_key | pass insert -m "machine/${MACHINE}/ssh-host-key/ed25519/private"
ssh "root@${IP}" cat /etc/ssh/ssh_host_rsa_key.pub | pass insert -m "machine/${MACHINE}/ssh-host-key/rsa/public"
ssh "root@${IP}" cat /etc/ssh/ssh_host_rsa_key | pass insert -m "machine/${MACHINE}/ssh-host-key/rsa/private"
```
6. run the installer script:
```
./installer/install-server.sh
```
7. Detach the boot disk and reboot.

You can use [nixos-anywhere](https://github.com/numtide/nixos-anywhere) to provision a new phyiscal machine. Create a new machine under `machines/personal-computers` or `machines/servers` then boot your new machine with any linux boot disk that provides you ssh access to `root`. 

A custom nixos install disk can be build with `nix build '.#nixos-iso'`. This ISO will include SSH public keys from `config/modules/security/sshd/public-keys.nix` as authorizedKeys for `root`.

Finally, run `nix run '.#install-pc'` or `nix run '.#install-server'` to provision the machine with `nixos-anywhere`. The script will prompt for the machine name and IP address.

NOTE: If you don't use the installer ISO and wish to deploy with `colmena` or provision with `nixos-anywhere` then be sure to provide your root user with an [authorized SSH key](https://github.com/solomon-b/nixos-config/blob/main/installer/configuration.nix#L48).

Once you have your machine provisioned you can use `colmena apply --on $MACHINE` to deploy it.

Lastly, if you are deploying with `colmena` then you will either need a DNS entry for your machine names or you will need to tweak the `mkMachine` function and set the IP with [deployment.targetHost](https://colmena.cli.rs/unstable/reference/deployment.html#deploymenttargethost).

### Deployment
Deployments are done with [colmena](https://colmena.cli.rs/unstable/reference/deployment.html#deploymenttargethost):

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
