{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
    defaultGateway = "165.227.48.1";
    defaultGateway6 = "";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="165.227.48.175"; prefixLength=20; }
          { address="10.46.0.6"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="fe80::60fb:65ff:fe1e:2ee3"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "165.227.48.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = ""; prefixLength = 32; } ];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="62:fb:65:1e:2e:e3", NAME="eth0"
    ATTR{address}=="0e:ac:ca:23:92:c8", NAME="eth1"
  '';
}
