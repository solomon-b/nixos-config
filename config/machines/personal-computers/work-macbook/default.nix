{ pkgs, ... }:

{
  # Match the names the corp-imaged machine already uses.
  networking.hostName = "DJGVXJ";
  networking.computerName = "DJGVXJ";
  networking.localHostName = "DJGVXJ";

  # nix-darwin needs to know which user owns user-scoped state (Homebrew,
  # `system.defaults`, etc.). Home Manager is wired to this same user in flake.nix.
  system.primaryUser = "solomon";
  users.users.solomon = {
    name = "solomon";
    home = "/Users/solomon";
  };

  # Nix here was installed by the Determinate Systems installer, which owns
  # /etc/nix/nix.conf (flakes already enabled). Hand Nix management back to it so
  # nix-darwin doesn't fight the installer.
  nix.enable = false;

  # This is a corp-imaged machine: leave the company-managed pieces of /etc alone.
  # The existing /etc/ssl/certs/ca-certificates.crt carries the internal/proxy CAs,
  # and sudo PAM is managed by MDM — don't let nix-darwin overwrite either.
  security.pki.installCACerts = false;
  security.pam.services.sudo_local.enable = false;

  # Make zsh a system-managed shell so the login shell picks up nix paths.
  programs.zsh.enable = true;

  # Bump only after reading the nix-darwin changelog for the migration notes.
  system.stateVersion = 5;
}
