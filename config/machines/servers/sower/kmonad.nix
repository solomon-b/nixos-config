{ ... }:

{
  primary-user.home-manager.programs.kmonad = {
    enable = true;

    defcfg = ''
      (defcfg
        input (device-file "/dev/input/event1")
        output (uinput-sink "external-keyboard")
        allow-cmd true
        fallthrough true
      )
    '';

    defsrc = ''
      (defsrc
        esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10   f11  f12       ssrq slck pause
        grv  1    2    3    4    5    6    7    8    9    0     -    =    bspc ins  home pgup
        tab  q    w    e    r    t    y    u    i    o    p     [    ]    \    del  end  pgdn
        caps a    s    d    f    g    h    j    k    l    ;     '    ret
        lsft z    x    c    v    b    n    m    ,    .     /    rsft
        lctl lmet lalt           spc                     ralt rmet cmp rctl         up
                                                                              left down rght
      )
    '';

    defaliases = ''
      (defalias
        tmt (tap-next tab lmet)
        \mt (tap-next \   rmet)
        xcp (tap-next esc lctl)
      )
    '';

    deflayers = ''
      (deflayer test
        esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10   f11  f12       ssrq slck pause
        grv  1    2    3    4    5    6    7    8    9    0     -    =    bspc ins  home pgup
        @tmt q    w    e    r    t    y    u    i    o    p     [    ]    @\mt del  end  pgdn
        @xcp a    s    d    f    g    h    j    k    l    ;     '    ret
        lsft z    x    c    v    b    n    m    ,    .     /    rsft
        lctl lmet lalt           spc                     ralt rmet cmp rctl         up
                                                                              left down rght
      )
    '';
  };

  primary-user.extraGroups = [ "uinput" "input" ];

  users.groups = { uinput = { }; };
  boot.kernelModules = [ "uinput" ];
  services.udev.extraRules = ''
    # KMonad user access to /dev/uinput
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';
}
