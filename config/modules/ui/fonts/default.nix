{ pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    fira-code
    material-design-icons
    noto-fonts-emoji
  ];
}
