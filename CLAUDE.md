# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Building and Deployment
- `nix build '.#nixos-iso'` - Build custom NixOS installer ISO with SSH keys
- `colmena apply --on @server` - Deploy all servers
- `colmena apply --on @pc` - Deploy all personal computers
- `colmena apply --on <machine-name>` - Deploy specific machine
- `nix run '.#deploy'` - Interactive deployment selector
- `nix run '.#deploy-all'` - Deploy all servers
- `nix run '.#install-pc'` - Provision new physical machine
- `nix run '.#install-server'` - Provision new virtual machine/server

### Development
- `nix develop` - Enter development shell with colmena and sops
- `nix flake update` - Update flake inputs
- `nixpkgs-fmt .` - Format Nix files
- `sops updatekeys --yes secrets.yaml` - Update SOPS encryption keys

## Architecture

This is a NixOS flake configuration managing a homelab with personal computers and servers using:

### Core Structure
- **config/machines/**: Machine-specific configurations
  - `personal-computers/`: Desktop PCs (lorean, voice-of-evening, nightshade)
  - `servers/`: Server configurations organized by function
- **config/profiles/**: Base configurations (pc, physical-machine, virtual-machine)
- **config/modules/**: Reusable NixOS modules (security, services, system, ui)
- **modules/**: Custom nixos and home-manager modules
- **installer/**: Custom installer ISO and provisioning scripts

### Key Technologies
- **Colmena**: Deployment orchestration
- **SOPS**: Secret management with age encryption
- **Disko**: Declarative disk partitioning
- **Home Manager**: User environment management
- **nixos-anywhere**: Remote machine provisioning

### Server Architecture
Each server has a specific role:
- **accompaniment-of-shadows**: nginx reverse proxy, docker orchestration
- **apollyon**: qBittorrent
- **madonna-of-the-wasps**: Tailscale exit node and subnet relay
- **silence-under-snow**: DNS server
- **sower**: Media streaming (Immich, Jellyfin, Podgrab, TubeArchivist, Navidrome)
- **storm-bird**: Monitoring (Prometheus/Grafana/Uptime)
- **transfigured-night**: PostgreSQL service

### Machine Provisioning Workflow
1. Create machine profile in `config/machines/`
2. Boot from custom ISO with SSH keys
3. Generate hardware config: `nixos-generate-config --show-hardware-config`
4. Add SSH host keys to `pass`
5. Generate age key and update `.sops.yaml`
6. Run installer script (`install-pc` or `install-server`)
7. Deploy with colmena

### Secret Management
- Secrets stored in `secrets.yaml` encrypted with SOPS
- Machine SSH host keys stored in `pass` under `machine/${MACHINE}/ssh-host-key/`
- Age keys derived from SSH host keys for SOPS encryption
- Primary user passwords managed through SOPS

### Home Manager Integration
- Personal computers use home-manager for user environment
- Configurations in `config/machines/personal-computers/*/home.nix`
- Standalone config for nightshade: `homeConfigurations.nightshade`