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
2. Boot the new machine off a linux boot disk where you have authorizedKeys for `root`.

You can build one with:
```
nix build '.#nixos-iso'
```
This ISO will include SSH public keys from `config/modules/security/sshd/public-keys.nix` as authorizedKeys for `root`.

3. For PCs, make sure the LUKS key and user SSH keys exist in `pass` before running the installer:
```
dd if=/dev/urandom bs=32 count=1 | pass insert -m "machine/${MACHINE}/luks/key/0"
ssh-keygen -t ed25519 -C "solomon@${MACHINE}" -f /tmp/ed25519_key
cat /tmp/ed25519_key | pass insert -m "machine/${MACHINE}/solomon/ssh/private-key"
cat /tmp/ed25519_key.pub | pass insert -m "machine/${MACHINE}/solomon/ssh/public-key"
```

4. Run the installer script. It handles hardware config generation, SSH host key creation, ZFS hostId generation, and runs [nixos-anywhere](https://github.com/numtide/nixos-anywhere) to provision the machine.

For physical machines:
```
nix run '.#install-pc'
```

For virtual machines:
```
nix run '.#install-server'
```

5. Derive an `age` key from the new machine's SSH host key and add it to `.sops.yaml`. Without this the machine can't decrypt secrets.
```
pass machine/${MACHINE}/ssh-host-key/ed25519/public | ssh-to-age
vim .sops.yaml
sops updatekeys --yes secrets.yaml
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
