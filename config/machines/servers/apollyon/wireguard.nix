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
      postUp = ''
        ip route add 192.168.1.0/24 via 192.168.5.1
      '';
      preDown = ''
        ip route delete 192.168.1.0/24
      '';

      peers = [ (import ./mullvad.nix).us-lax-wg-303 ];
    };
  };

  sops.secrets.primary-user-wireguard-private-key = { };

  # We must enforce the order for service launch of tailscale and
  # wireguard to set the correct IP rule prioritization.
  # https://rakhesh.com/linux-bsd/tailscale-wireguard-co-existing-or-i-love-policy-based-routing/
  #systemd.services.tailscaled.after = ["wg-quick-wg0.service"];
}
