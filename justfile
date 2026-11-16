# Show the recipes.
default:
    @just --list

################################################################################
# Deployment

# Deploy one machine or one tag. Examples: `just deploy sower`, `just deploy @server`.
deploy target:
    colmena apply --on {{ target }}
    just failed {{ target }}

# Select a server from a menu, then deploy it.
deploy-pick:
    nix run '.#deploy'

# Deploy all servers.
deploy-servers: (deploy "@server")

# Deploy all personal computers.
deploy-pcs: (deploy "@pc")

# Build one machine or one tag. This recipe does not deploy.
build target:
    colmena build --on {{ target }}

# List the failed systemd units. Colmena reports success when a unit fails.
failed target:
    colmena exec --on {{ target }} -- systemctl list-units --state=failed

# Apply the nix-darwin configuration on work-macbook.
darwin:
    darwin-rebuild switch --flake '.#work-macbook'

# Apply the standalone home-manager configuration on nightshade.
home:
    home-manager switch --flake '.#nightshade'

################################################################################
# Provisioning

# Build the installer ISO.
iso:
    nix build '.#nixos-iso'

# Provision a new machine.
install:
    nix run '.#install'

################################################################################
# Maintenance

# Format the Nix files.
fmt:
    nixpkgs-fmt .

# Update the flake inputs.
update:
    nix flake update

# Encrypt secrets.yaml again for the keys in .sops.yaml.
updatekeys:
    sops updatekeys --yes secrets.yaml

# Find pinned container images that have a newer release.
check-pins:
    nix run '.#check-pins'
