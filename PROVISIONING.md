# VM provisioning on TrueNAS

The installer (`nix run '.#install'`) can create a bhyve VM on the
TrueNAS box, install NixOS on it, and get it ready for colmena.

## TrueNAS facts

- Host: `sandra-voi.home.arpa`, TrueNAS CORE 13.0.
- API key: in `pass` at `service/truenas/api/nixos-installer`.
- VM disks: thick zvols in the pool `virtual-machines`.
- New VMs match the existing VMs: UEFI boot, VirtIO disk, VirtIO NIC on
  `igb0`, and a VNC console with a password.

## Make a new VM

1. Run `nix run '.#install'` from the repo root.
2. Pick "server", then "virtual machine", then "+ create new". Type the
   machine name. Use lowercase letters, digits, and hyphens.
3. Say yes to "Create this VM on TrueNAS?".
4. Enter the guest CPU count (an even number), the RAM in GiB, and the
   disk size in GiB. The defaults are 2, 4, and 50.
5. The installer copies the installer ISO to the NAS if it is not there.
   Then it creates the zvol and the VM. The VM does not start yet.
6. The installer prints the MAC address of the VM. Make a fixed IP
   reservation for that MAC in the UniFi web UI. Then enter the IP.
7. The installer starts the VM and waits for SSH on the ISO.
8. The normal install runs: hardware detection, host keys,
   `nixos-anywhere`, and SOPS registration. The VM reboots from its
   disk, and the installer removes the ISO from the VM.
9. Commit the new files under `config/machines/servers/<name>/`, and
   `.sops.yaml` and `secrets.yaml` if they changed.
10. Give the VM a DNS name. Add `<ip> <name>` to both host blocks in
    `config/machines/servers/accompaniment-of-shadows/coredns.nix` and
    deploy accompaniment-of-shadows. A VM on the tailnet gets its name
    from Tailscale instead.
11. Run `colmena apply --on <name>`.

The flake finds every directory under `config/machines/servers/`, so a
new server needs no change to `flake.nix`.

The VNC password for the TrueNAS console is in `pass` at
`machine/<name>/vnc-password`.

## If the install fails

Run the installer again and pick the same machine. The installer marks
the VMs that it creates (the description is
`created by nixos-config installer`). If it finds its own VM, it uses
it and continues from the IP step.

The installer never changes a VM or zvol that it did not create.

## Use an existing VM

Say no to "Create this VM on TrueNAS?". The installer then asks for the
IP address, and the flow is the same as for any other machine. Boot the
VM from the installer ISO (`nix build '.#nixos-iso'`) first.

## Remove a VM that the installer made

1. In the TrueNAS web UI, stop and delete the VM.
2. Delete the zvol `virtual-machines/<name>`.
3. Remove `config/machines/servers/<name>/` and commit.
4. Remove the machine from `.sops.yaml`, then run
   `sops updatekeys --yes secrets.yaml`.
5. Run `pass rm -r machine/<name>`.
6. Remove the UniFi reservation and the coredns line.
