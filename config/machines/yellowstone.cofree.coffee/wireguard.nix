{ config, pkgs, ... }:
let
  passwords = pkgs.callPackage ../../../lib/passwords.nix { };
in
{
  environment.systemPackages = [ pkgs.wireguard pkgs.wireguard-tools ];

  # enable NAT
  networking.nat.enable = true;
  networking.nat.externalInterface = "eth0";
  networking.nat.internalInterfaces = [ "wg0" ];
  networking.firewall = {
    allowedUDPPorts = [ 51820 ];
  };

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.1/24" ];
      listenPort = 51820;

      # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
      # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
      '';

      # This undoes the above command
      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
      '';

      #privateKeyFile = config.deployment.keys.wireguard-private-key.path;
      # "system/yellowstone/wireguard/private-key"
      privateKeyFile = "/secrets/primary-user-wireguard-private-key";

      peers = [
        { publicKey = config.primary-user.wireguardPubKey;
          allowedIPs = [ "10.100.0.2/32" ];
        }
        { publicKey = config.primary-user.android.wireguardPubKey;
          allowedIPs = [ "10.100.0.3/32" ];
        }
        # Sower's pubkey
        # Fetch password from pass
        {
          #publicKey = builtins.extraBuiltins.getFullPasswordValue pkgs "system/sower/wireguard/public-key";
        { publicKey = "Y6OqeDXON8DZ83Hf4yGBekMWDtIPRzyvVxg0M9zqZxg=";
          allowedIPs = [ "10.100.0.4/32" ];
        }
      ];
    };
  };
}
