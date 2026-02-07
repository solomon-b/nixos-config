{ pkgs, ... }:

{
  fonts.packages = with pkgs; [
    fira-code
    iosevka
    material-design-icons
    noto-fonts-color-emoji
    stix-two
  ];
}
