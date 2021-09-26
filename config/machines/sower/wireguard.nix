{ config, pkgs, ... }:
let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
in
{
  deployment.keys = {
    wireguard-private-key = {
      keyCommand = passwords.getPassword "system/sower/wireguard/private-key";
      destDir = "/secrets";
      user = config.primary-user.name;
    };
  };

  environment.systemPackages = [ pkgs.wireguard pkgs.wireguard-tools ];

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

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.4/24" ];
      listenPort = 51820; # to match firewall allowedUDPPorts (without this wg uses random port numbers)
      privateKeyFile = config.deployment.keys.wireguard-private-key.path;

      peers = [
        {
          # Fetch password from pass
          publicKey = builtins.extraBuiltins.getFullPasswordValue pkgs "system/yellowstone/wireguard/public-key";
          allowedIPs = [ "0.0.0.0/0" "::/0" ];
          endpoint = "yellowstone.cofree.coffee:51820";
          persistentKeepalive = 25;
        }
      ];
    };
  };
}
