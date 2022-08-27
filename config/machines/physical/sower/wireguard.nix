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
      address = [ "10.65.202.199/32" "fc00:bbbb:bbbb:bb01::2:cac6/128" ];
      dns = [ "193.138.218.74" ];
      privateKeyFile = "/secrets/primary-user-wireguard-private-key-mulvad-257";
      
      peers = [
        {
          publicKey = "ELo9g48V0FSx8fgpAPKcSlRf1my+vagqI9J26IRLIC4=";
          allowedIPs = [ "0.0.0.0/0" "::0/0" ];
          endpoint = "37.19.210.1:51820";
          persistentKeepalive = 25;
        }
      ];
    };
  };

  # We must enforce the order for service launch of tailscale and
  # wireguard to set the correct IP rule prioritization.
  # https://rakhesh.com/linux-bsd/tailscale-wireguard-co-existing-or-i-love-policy-based-routing/
  systemd.services.tailscaled.after = ["wg-quick-wg0.service"];
}
