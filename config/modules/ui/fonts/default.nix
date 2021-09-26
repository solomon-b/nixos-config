{ pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    font-awesome
    fira-code
    noto-fonts-emoji
  ];
}
