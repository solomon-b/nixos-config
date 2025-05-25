{ pkgs, lib, config, ... }:
let
  dmenu = pkgs.stdenv.mkDerivation {
    name = "dmenu-wrapper";

    dontUnpack = true;

    nativeBuildInputs = [ pkgs.makeWrapper ];

    installPhase = ''
      mkdir -p $out/bin
      makeWrapper ${pkgs.dmenu}/bin/dmenu $out/bin/dmenu \
        --add-flags "-nb '#2d2d2d' -nf '#cccccc' -sb '#333333' -sf '#cccccc'"

      makeWrapper ${pkgs.dmenu}/bin/dmenu_run $out/bin/dmenu_run \
        --add-flags "-l 30 -nb '#2d2d2d' -nf '#cccccc' -sb '#333333' -sf '#cccccc'"

      cp ${pkgs.dmenu}/bin/dmenu_path $out/bin/dmenu_path
    '';
  };

  pass-menu = pkgs.stdenv.mkDerivation {
    name = "passmenu-wrapper";
    dontUnpack = true;
    nativeBuildInputs = [ pkgs.makeWrapper ];

    installPhase = ''
      mkdir -p $out/bin

      makeWrapper ${pkgs.pass}/bin/passmenu $out/bin/passmenu \
        --add-flags "-l 30 -nb '#2d2d2d' -nf '#cccccc' -sb '#333333' -sf '#cccccc'"
    '';
  };

  emoji-prompt = pkgs.writeScriptBin "emoji-prompt" ''
    cat ${./emojis} | ${dmenu}/bin/dmenu -l 30 | cut -d ' ' -f 1 | xclip -sel clip
  '';

  exit-prompt = (pkgs.writeScriptBin "exit-prompt" (builtins.readFile ./exit-prompt.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });

  kill-window-prompt = (pkgs.writeScriptBin "kill-window-prompt" (builtins.readFile ./kill-window-prompt.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });

  screenshot-prompt = (pkgs.writeScriptBin "screenshot-prompt" (builtins.readFile ./screenshot-prompt.sh)).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
{
  home.packages = [
    dmenu
    emoji-prompt
    exit-prompt
    kill-window-prompt
    pass-menu
    pkgs.networkmanager_dmenu
    screenshot-prompt
  ];

  xdg.configFile."networkmanager-dmenu/config.ini".text = ''
    [dmenu]
    dmenu_command = dmenu -l 30
    active_chars = ==
    highlight = True
    highlight_fg =
    highlight_bg =
    highlight_bold = True
    compact = False
    pinentry = pinentry-gtk-2
    wifi_icons = 󰤯󰤟󰤢󰤥󰤨
    format = {name:<{max_len_name}s}  {sec:<{max_len_sec}s} {icon:>4}
    list_saved = False
    prompt = Networks

    [dmenu_passphrase]
    obscure = False
    obscure_color = #222222

    [pinentry]
    description = Get network password
    prompt = Password:

    [editor]
    terminal = xterm
    gui_if_available = True
    gui = nm-connection-editor

    [nmdm]
    rescan_delay = 5
  '';
}
