{ config, pkgs, lib, ... }:

{
  # Ghostty is installed out-of-band (Homebrew cask on the corp Mac), so this
  # module only manages config — it does not pull ghostty from nixpkgs.
  #
  # Ghostty resolves a `theme = <name>` directive against its theme search path,
  # which includes ~/.config/ghostty/themes. We ship the Sanity Inc Tomorrow
  # Night Eighties palette there (matching the `st` theme on the Linux PCs) and
  # reference it by name from the main config.
  xdg.configFile = {
    "ghostty/themes/sanityinc-tomorrow-eighties".source =
      ./sanityinc-tomorrow-eighties;

    "ghostty/config".text = ''
      theme = sanityinc-tomorrow-eighties

      # Always save and restore window state
      window-save-state = always

      # Enable shell integration for working directory preservation
      shell-integrations = detect
      shell-integration-features = no-sudo,cursor,title

      # Control auto-update behavior to avoid surprise restarts
      auto-update = check
    '';
  };
}
