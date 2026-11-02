{ config, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.wireguard-tools ];

  networking.firewall = {
    allowedUDPPorts = [ 51820 ];

    # if packets are still dropped, they will show up in dmesg
    logReversePathDrops = true;
  };

  networking.wg-quick.interfaces = {
    wg0 = {
      address = [ "10.64.123.129/32" "fc00:bbbb:bbbb:bb01::3:9d1e/128" ];
      dns = [ "10.64.0.1" ];
      privateKeyFile = config.sops.secrets.primary-user-wireguard-private-key.path;
      # wg-quick sends all traffic to wg0 at rule priority 5209, which is
      # before the Tailscale rules. The rules at 5200 send tailnet traffic to
      # the Tailscale table (52). When Tailscale is down, table 52 is empty
      # and the traffic continues to wg0.
      postUp = ''
        ip route add 192.168.1.0/24 via 192.168.5.1
        ip rule add to 100.64.0.0/10 lookup 52 priority 5200
        ip -6 rule add to fd7a:115c:a1e0::/48 lookup 52 priority 5200
      '';
      preDown = ''
        ip route delete 192.168.1.0/24
        ip rule delete to 100.64.0.0/10 lookup 52 priority 5200
        ip -6 rule delete to fd7a:115c:a1e0::/48 lookup 52 priority 5200
      '';

      peers = [ (import ./mullvad.nix).us-lax-wg-203 ];
    };
  };

  sops.secrets.primary-user-wireguard-private-key = { };

  # Kill switch. When wg0 goes down, wg-quick removes its routing rules and
  # traffic falls back to the LAN default route. qBittorrent must stop with
  # the tunnel so that torrent traffic never leaves on the home IP.
  systemd.services.qBittorrent = {
    bindsTo = [ "wg-quick-wg0.service" ];
    after = [ "wg-quick-wg0.service" ];
    partOf = [ "wg-quick-wg0.service" ];
  };
}
