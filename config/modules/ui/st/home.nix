{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    (st.overrideAttrs (oldAttrs: rec {
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
