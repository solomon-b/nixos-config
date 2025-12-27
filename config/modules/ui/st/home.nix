{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Pinned to 0.9.2 because patches aren't compatible with 0.9.3 yet.
    # Consider migrating to st-flexipatch: https://github.com/bakkeby/st-flexipatch
    (st.overrideAttrs (oldAttrs: rec {
      version = "0.9.2";
      src = fetchurl {
        url = "https://dl.suckless.org/st/st-${version}.tar.gz";
        sha256 = "sha256-ayFdT0crIdYjLzDyIRF6d34kvP7miVXd77dCZGf5SUs=";
      };
      buildInputs = oldAttrs.buildInputs ++ [ harfbuzz ];
      patches = [
        ./st-scrollback-0.9.2.diff
        ./st-scrollback-reflow-0.9.2.diff
        ./st-scrollback-mouse-20220127-2c5edf2.diff
        ./st-scrollback-mouse-altscreen-20220127-2c5edf2.diff
        ./st-blinking_cursor-20230819-3a6d6d7.diff
        #./st-ligatures-scrollback-20240427-0.9.2.diff
        ./0001-Adds-Fira-Code-Font.patch
        ./0002-Adds-Sanity-Inc-Tomorrow-Eighties-Theme.patch
        ./0003-Change-Font-Size-Shortcuts.patch
      ];
    }))
  ];
}
