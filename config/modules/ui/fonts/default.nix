{ pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    fira-code
    iosevka
    material-design-icons
    noto-fonts-emoji
  ];
}
