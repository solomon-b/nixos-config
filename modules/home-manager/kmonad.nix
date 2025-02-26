{ pkgs, ... }:

let
  config-file = device: pkgs.writeText "kmonad.cfg"
    ''
      (defcfg
        input (device-file "${device}")
        output (uinput-sink "internal-keyboard")
        allow-cmd true
        fallthrough true
      )

      (defsrc
        esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10   f11  f12  home end  ins  del
        grv  1    2    3    4    5    6    7    8    9    0     -    =    bspc
        tab  q    w    e    r    t    y    u    i    o    p     [    ]    \
        caps a    s    d    f    g    h    j    k    l    ;     '    ret
        lsft z    x    c    v    b    n    m    ,    .     /    rsft
        lctl  lalt lmet           spc            ralt prnt rctl   pgup up   pgdn
                                                                  left down rght
      )

      (defalias
        tmt (tap-next tab lmet)
        \mt (tap-next \   rmet)
        xcp (tap-next esc lctl)
      )

      (deflayer test
        esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10   f11  f12  home end  ins  del
        grv  1    2    3    4    5    6    7    8    9    0     -    =    bspc
        @tmt q    w    e    r    t    y    u    i    o    p     [    ]    @\mt
        @xcp a    s    d    f    g    h    j    k    l    ;     '    ret
        lsft z    x    c    v    b    n    m    ,    .     /    rsft
        lctl  lalt lmet           spc            ralt prnt rctl   pgup up   pgdn
                                                                  left down rght
      )
    '';
in
{
  #primary-user.extraGroups = [ "uinput" "input" ];

  #users.groups = { uinput = { }; };
  #boot.kernelModules = [ "uinput" ];
  #services.udev.extraRules = ''
  #  # KMonad user access to /dev/uinput
  #  KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  #'';

  systemd.user.services.kmonad-internal = {
    Unit = {
      description = "kmonad internal keyboard config";
    };

    Install = {
      wantedBy = [ "graphical.target" ];
    };
    
    Service = {
      Restart = "always";
      RestartSec = "3";
      ExecStart = "${pkgs.kmonad}/bin/kmonad ${config-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd"}";
    };
  };
}

