{ config, pkgs, lib, ... }:

{
  imports = [
    # OS-agnostic — sets username/home dir (isDarwin -> /Users/solomon).
    ../../../../modules/home-manager/primary-user.nix

    # Cross-platform shell + tooling shared with the Linux machines.
    ../../../modules/ui/zsh/home.nix
    ../../../modules/ui/git/home.nix
    ../../../modules/ui/starship/home.nix
    ../../../modules/ui/ghostty/home.nix
    # NOTE: direnv is intentionally NOT imported here. Its prompt hook spawns a
    # `direnv export zsh` subprocess on every prompt, and this corp Mac's
    # endpoint-security stack (Santa + SentinelOne) taxes every exec ~270ms.
  ];

  # This is a work machine: commit as the work identity instead of the personal
  # email baked into the shared git module.
  programs.git.settings.user.email = lib.mkForce "solomon.bothwell@kraken.com";

  # Endpoint security (Santa + SentinelOne) adds ~270ms to EVERY process exec on
  # this machine, so the felt cost of the prompt is dominated by how many
  # subprocesses it spawns. Disable the starship modules that shell out to git on
  # every render; git_branch (in-process) stays. Saves ~1s/prompt inside a repo.
  programs.starship.settings = {
    command_timeout = 200;
    git_status.disabled = true; # runs `git status`
    git_metrics.disabled = true; # runs `git diff --shortstat`
  };

  programs.bash.enable = true;

  home.packages = with pkgs; [
    # General CLI
    fzf
    ispell
    nix-search
    tmux
    worktrunk
  ];

  # Homebrew lives at /opt/homebrew (user/corp install, not managed by nix-darwin).
  # home-manager relocated ZDOTDIR to ~/.config/zsh, so the installer's brew line in
  # ~/.zprofile is no longer read. Re-expose brew by setting its env statically
  # rather than `eval "$(brew shellenv)"`, which would spawn brew on every shell
  # start (expensive given this machine's ~270ms exec tax).
  # Karabiner-Elements (installed via Homebrew) config. Caps Lock acts as Control
  # when held and Escape when tapped. Managed declaratively, so the Karabiner GUI
  # can't persist its own edits to this file — change the mapping here instead.
  #
  # force = true: Karabiner writes a default karabiner.json on first launch, which
  # home-manager would otherwise refuse to clobber. This lets our version win.
  # Karabiner reads rules fine from the read-only symlink; it just can't save GUI
  # changes back (it may log a harmless "cannot save" — expected).
  xdg.configFile."karabiner/karabiner.json" = {
    source = ./karabiner.json;
    force = true;
  };

  # ~/.local/bin was added to PATH by the corp ~/.zprofile / ~/.zshrc, but
  # home-manager relocated ZDOTDIR to ~/.config/zsh so those files are no longer
  # sourced (same reason the brew line broke). Re-declare it here.
  home.sessionPath = [ "$HOME/.local/bin" "/opt/homebrew/bin" "/opt/homebrew/sbin" ];
  home.sessionVariables = {
    EDITOR = "vim";
    HOMEBREW_PREFIX = "/opt/homebrew";
    HOMEBREW_CELLAR = "/opt/homebrew/Cellar";
    HOMEBREW_REPOSITORY = "/opt/homebrew";
  };
}
