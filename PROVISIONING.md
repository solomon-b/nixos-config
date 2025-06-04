# VM Provisioning Guide for TrueNAS/bhyve

This guide walks you through provisioning a new virtual machine in your NixOS homelab setup using TrueNAS with bhyve virtualization.

## Prerequisites

1. **TrueNAS VM running** with bhyve virtualization
2. **Development environment** with this flake repo
3. **Pass password store** set up with existing secrets
4. **SSH access** to your development machine from TrueNAS

## Step 1: Create VM in TrueNAS/bhyve

1. **Create VM in TrueNAS interface:**
   - Guest OS: Linux
   - Memory: 2GB+ (adjust based on services)
   - CPU: 2+ cores
   - Storage: Create virtual disk (20GB+ recommended)
   - Network: Bridge to your home network

2. **Boot from NixOS ISO:**
   ```bash
   # Build the custom installer ISO with your SSH keys
   nix build '.#nixos-iso'
   # Mount the ISO to the VM and boot
   ```

## Step 2: Prepare Machine Configuration

1. **Choose a name for your new server** (following the poetic naming convention):
   ```bash
   NEW_MACHINE="your-server-name"  # e.g., "whisper-in-twilight"
   ```

2. **Create machine directory:**
   ```bash
   mkdir -p "config/machines/servers/${NEW_MACHINE}"
   ```

3. **Create basic default.nix:**
   ```bash
   cat > "config/machines/servers/${NEW_MACHINE}/default.nix" << EOF
   { pkgs, ... }:

   {
     imports = [
       ./hardware.nix
       
       ../../../profiles/virtual-machine
     ];

     networking.hostName = "${NEW_MACHINE}";
     
     # Add service-specific imports here
     # ./some-service.nix
   }
   EOF
   ```

4. **Choose disk configuration** - copy one of these templates:

   **For simple LVM setup (recommended for most VMs):**
   ```bash
   cp config/machines/servers/test-vm/disk-config.nix "config/machines/servers/${NEW_MACHINE}/"
   # Edit device path if needed (bhyve typically uses /dev/vda)
   ```

   **For ZFS setup (if you need snapshots/compression):**
   ```bash
   cp config/machines/servers/gnostic-ascension/disk-config.nix "config/machines/servers/${NEW_MACHINE}/"
   ```

## Step 3: Get VM IP Address

1. **Find the VM's IP** after it boots from ISO:
   ```bash
   # Check TrueNAS VM console or your router's DHCP leases
   VM_IP="192.168.x.x"  # Replace with actual IP
   ```

2. **Test SSH access:**
   ```bash
   ssh "root@${VM_IP}"
   # Should work due to SSH keys in the ISO
   ```

## Step 4: Run Automated Provisioning

1. **Run the install script:**
   ```bash
   nix run '.#install-server'
   # Select your new machine from the list
   # Enter the VM IP when prompted
   ```

   **The script will automatically:**
   - Generate random hostId for ZFS
   - Generate hardware.nix from the VM
   - Generate NEW SSH host keys for the machine
   - Store the SSH keys in your pass password store
   - Deploy NixOS with your configuration

## Step 5: Configure Secrets

1. **Generate age key for SOPS:**
   ```bash
   pass machine/${NEW_MACHINE}/ssh-host-key/ed25519/public | ssh-to-age
   # Copy the output key
   ```

2. **Add to .sops.yaml:**
   ```bash
   vim .sops.yaml
   # Add the age key under creation_rules
   ```

3. **Update secrets:**
   ```bash
   sops updatekeys --yes secrets.yaml
   ```

## Step 6: Deploy and Configure

1. **Detach ISO and reboot VM** in TrueNAS interface

2. **Wait for VM to boot**, then deploy your configuration:
   ```bash
   colmena apply --on ${NEW_MACHINE}
   ```

3. **Add services** by editing the machine's default.nix:
   ```nix
   imports = [
     ./hardware.nix
     ./your-service.nix  # Add service configs
     ../../../profiles/virtual-machine
   ];
   ```

## Available Disk Configuration Templates

### Simple LVM (test-vm template)
- **Best for**: Most general-purpose VMs
- **Storage**: Single ext4 filesystem on LVM
- **Device**: `/dev/sda` (update to `/dev/vda` for bhyve)
- **Partitions**: 1MB boot + 500MB ESP + 100% LVM root

### Advanced ZFS (gnostic-ascension template)
- **Best for**: VMs needing snapshots, compression, or data integrity
- **Storage**: ZFS pool with separate datasets
- **Device**: `/dev/vda` (ideal for bhyve)
- **Features**: 
  - Automatic snapshots
  - zstd compression
  - Separate datasets for root, nix, home, logs
  - 2GB reservation to prevent pool filling

## Troubleshooting

**If VM doesn't boot:**
- Check bhyve logs in TrueNAS
- Verify disk configuration matches VM setup
- Ensure VM has sufficient resources

**If SSH fails:**
- Check firewall settings in TrueNAS
- Verify SSH keys are in the custom ISO
- Try console access through TrueNAS interface

**If deployment fails:**
- Check `colmena apply --on ${NEW_MACHINE} --verbose`
- Verify secrets are properly configured
- Check DNS resolution for the hostname

**Common Issues:**
- **Device path mismatch**: bhyve VMs typically use `/dev/vda`, update disk-config.nix accordingly
- **Network connectivity**: Ensure VM network is bridged to your home network
- **Resource constraints**: VMs need adequate RAM and CPU for the services they'll run

## Post-Deployment

After successful deployment, your VM will be:
- ✅ Accessible via SSH as user `solomon`
- ✅ Monitored via Prometheus (port 9002)
- ✅ Connected to Tailscale VPN
- ✅ Ready for service-specific configuration

Remember to update your monitoring and backup configurations to include the new VM.