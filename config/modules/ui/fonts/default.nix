{ pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    feather
    fira-code
    iosevka
    material-design-icons
    noto-fonts-emoji
  ];
}
