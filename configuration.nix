{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Use the GRUB 2 Boot Loader
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";

  # Allow non-free packages
  nixpkgs.config.allowUnfree = true;

  # Set number of cores
  nix.buildCores = 2;

  nix.trustedUsers = [ "@wheel" ];
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

  powerManagement.powertop.enable = true;

  environment.systemPackages = with pkgs; [
    dunst
    dmenu
    libnotify
    networkmanagerapplet
    termonad-with-packages
    trayer
    udiskie
    xbanish
    xlayoutdisplay
    wireguard
  ];

  virtualisation.docker.enable = true;
  services.redis.enable = true;
  #services.sshd.enable = true;

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "user-with-access-to-virtualbox" ];

  services.syncthing = {
    enable = true;
    user = "solomon";
    dataDir = "/home/solomon/Public";
    configDir = "/home/solomon/.config/syncthing";
  };

  users.extraUsers.solomon = {
    home = "/home/solomon";
    shell = pkgs.zsh;
    extraGroups = [ "audio" "sound" "docker" "networkmanager" "wheel" "input" "uinput"];
    isNormalUser = true;
    uid = 1000;
  };

  environment.shells = [pkgs.zsh];

  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    #wireless = {
    #  enable = true;
    #  networks = {
    #    TP-LINK_0573_5G = {
    #      pskRaw = "43c10b524fcb5286ff0242495ad099a837ff5fe9836bbffa1ae1831303884b7d";
    #    };
    #  };
    #};

    useDHCP = false;
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp4s0.useDHCP = true;
    hosts = {
      "192.168.0.3" = [ "sower" ];
    };
  };

  users.users.localtimed.group = "localtimed";
  users.groups.localtimed = {};
  services.localtime.enable = true;
  #time.timeZone = "America/Los_Angeles";

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };

  # Sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  #hardware.pulseaudio.configFile = pkgs.writeText "default.pa" ''
  #  load-module module-bluetooth-policy
  #  load-module module-bluetooth-discover
  #  ## module fails to load with
  #  ##   module-bluez5-device.c: Failed to get device path from module arguments
  #  ##   module.c: Failed to load module "module-bluez5-device" (argument: ""): initialization failed.
  #  # load-module module-bluez5-device
  #  # load-module module-bluez5-discover
  #'';
  #hardware.pulseaudio.extraConfig = "
  #  load-module module-switch-on-connect
  #";


  # OpenGL
  hardware.opengl.driSupport32Bit = true;

  # X11 windowing system
  services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "ctrl:nocaps";
      #windowManager.session = [
      #  {
      #    name = "xmonad";
      #    start = ''
      #      /usr/bin/env xmonad-solomon &
      #      waitPID=$!
      #    '';
      #  }
      #];
      windowManager.xmonad.enable = true;
      windowManager.xmonad.extraPackages =
        haskellPackages: [
          haskellPackages.xmonad-contrib
        ];
      displayManager = {
        defaultSession = "none+xmonad";
        lightdm.enable = true;
        lightdm.background = "/usr/share/backgrounds/Vaporwave.jpg";
      };
  };

  services.xserver.libinput = {
      enable = true;
      touchpad = {
        clickMethod = "clickfinger";
        tapping = false;
        disableWhileTyping = true;
      };
  };

  services.journald.extraConfig =
    ''
    SystemMaxUse=50M
    '';

  # KMonad
  users.groups = { uinput = {}; };
  boot.kernelModules = [ "uinput" ];
  services.udev.extraRules =
    ''
      # KMonad user access to /dev/uinput
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_12;
    settings = {
      shared_preload_libraries = "pg_stat_statements";
    };
  };

  fonts.fonts = with pkgs; [
    font-awesome
    fira-code
    noto-fonts-emoji
  ];

  # Nix Store Related
  system.stateVersion = "20.09";
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";
  nix.gc.dates = "03:15";
  nix.autoOptimiseStore = true;

  # Wireguard Related
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];

    # if packets are still dropped, they will show up in dmesg
    logReversePathDrops = true;

    # wireguard trips rpfilter up
    extraCommands = ''
      ip46tables -t raw -I nixos-fw-rpfilter -p udp -m udp --sport 51820 -j RETURN
      ip46tables -t raw -I nixos-fw-rpfilter -p udp -m udp --dport 51820 -j RETURN
    '';
    extraStopCommands = ''
      ip46tables -t raw -D nixos-fw-rpfilter -p udp -m udp --sport 51820 -j RETURN || true
      ip46tables -t raw -D nixos-fw-rpfilter -p udp -m udp --dport 51820 -j RETURN || true
    '';
  };

    # Enable WireGuard
  networking.wireguard.interfaces = {
    # "wg0" is the network interface name. You can name the interface arbitrarily.
    wg0 = {
      # Determines the IP address and subnet of the client's end of the tunnel interface.
      ips = [ "10.100.0.2/24" ];
      listenPort = 51820; # to match firewall allowedUDPPorts (without this wg uses random port numbers)

      # Path to the private key file.
      #
      # Note: The private key can also be included inline via the privateKey option,
      # but this makes the private key world-readable; thus, using privateKeyFile is
      # recommended.
      privateKeyFile = "/home/solomon/.wireguard/private.key";

      peers = [
        # For a client configuration, one peer entry for the server will suffice.

        {
          # Public key of the server (not a file path).
          publicKey = "Y6OqeDXON8DZ83Hf4yGBekMWDtIPRzyvVxg0M9zqZxg=";

          # Forward all the traffic via VPN.
          allowedIPs = [ "0.0.0.0/0" "::/0" ];
          # Or forward only particular subnets
          #allowedIPs = [ "10.100.0.1" "91.108.12.0/22" ];

          # Set this to the server IP and port.
          endpoint = "yellowstone.cofree.coffee:51820"; # ToDo: route to endpoint not automatically configured https://wiki.archlinux.org/index.php/WireGuard#Loop_routing https://discourse.nixos.org/t/solved-minimal-firewall-setup-for-wireguard-client/7577

          # Send keepalives every 25 seconds. Important to keep NAT tables alive.
          persistentKeepalive = 25;
        }
      ];
    };
  };
  # End Wireguard Related

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
}
