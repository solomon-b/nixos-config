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
      address = [ "10.67.117.137/32" "fc00:bbbb:bbbb:bb01::4:7588/128" ];
      dns = [ "10.64.0.1" ];
      privateKeyFile = "/secrets/primary-user-wireguard-private-key-mulvad-257";
      postUp = ''
        ip route add 192.168.1.0/24 via 192.168.5.1
      '';
      preDown = ''
        ip route delete 192.168.1.0/24
      '';
      
      peers = [
        {
          publicKey = "9BP/NfyAc1gljj86BXgu0/DcP5nK9OismCKxX70kO2k=";
          allowedIPs = [ "0.0.0.0/0" "::0/0" ];
          endpoint = "37.19.210.27:51820";
          persistentKeepalive = 25;
        }
      ];
    };
  };

  # We must enforce the order for service launch of tailscale and
  # wireguard to set the correct IP rule prioritization.
  # https://rakhesh.com/linux-bsd/tailscale-wireguard-co-existing-or-i-love-policy-based-routing/
  #systemd.services.tailscaled.after = ["wg-quick-wg0.service"];
}
