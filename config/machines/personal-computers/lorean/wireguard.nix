{ config, pkgs, ... }:
{
  environment.systemPackages = [ pkgs.wireguard-tools ];

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

  #networking.wireguard.interfaces = {
  #  wg0 = {
  #    ips = [ "10.100.0.4/24" ];
  #    listenPort = 51820; # to match firewall allowedUDPPorts (without this wg uses random port numbers)
  #    privateKeyFile = "/secrets/primary-user-wireguard-private-key";
  #
  #    peers = [
  #      {
  #        # TODO: Fetch password from pass
  #        #publicKey = builtins.extraBuiltins.getFullPasswordValue pkgs "system/yellowstone/wireguard/public-key";
  #        publicKey = "Y6OqeDXON8DZ83Hf4yGBekMWDtIPRzyvVxg0M9zqZxg=";
  #        allowedIPs = [ "0.0.0.0/0" "::/0" ];
  #        endpoint = "yellowstone.cofree.coffee:51820";
  #        persistentKeepalive = 25;
  #      }
  #    ];
  #  };
  #};
}
