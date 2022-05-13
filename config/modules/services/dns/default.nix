{ config, pkgs, ... }:

{
  services.dnsmasq = {
    enable = true;
    extraConfig = ''
      bogus-priv
      domain-needed
      no-dhcp-interface=eno1
      no-hosts
      no-poll
      no-resolv

      domain=galaxybrain.zone
      local=/galaxybrain.zone/

      listen-address=::1,127.0.0.1,192.168.0.3
      bind-interfaces

      cache-size=10000

      server=8.8.8.8
      server=8.8.4.4

      address=/sower/192.168.0.3
      address=/sower.galaxybrain.zone/192.168.0.3
      address=/router/192.168.0.1
      address=/router.galaxybrain.zone/192.168.0.1
    '';
  };

  networking = {
    firewall = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };
  };
}
     # log-queries
     # log-facility=/tmp/ad-block.log
     # local-ttl=300
 
     # conf-file=/etc/nixos/assets/hosts-blocklists/domains.txt
     # addn-hosts=/etc/nixos/assets/hosts-blocklists/hostnames.txt

#let
#  inherit (config.networking) domain hostName;
#  fqdb = "${hostName}.${domain}";
#in
#{
#  networking.firewall = {
#    allowedTCPPorts = [ 53 ];
#    allowedUDPPorts = [ 53 ];
#  };
#
#  interfaces.docker0 = {
#    allowedTCPPorts = [ 5053 ];
#    allowedUDPPorts = [ 5053 ];
#  };
#
#
#  services.nginx.virtualHosts."pihole.${fqdn}" = {
#    forceSSL = true;
#    useACMEHost = domain;
#    locations."/".proxyPass = "http://localhost:7000";
#  };
#
#  virtualisation.oci-containers.containers.pihole = {
#    image = "pihole/pihole:latest";
#    ports = [
#      "53:53/tcp"
#      "53:53/udp"
#      "7000:80"
#    ];
#    volumes = [ "/etc/pihole:/etc/pihole/" ];
#    environment = {
#      DNS1 = "10.88.0.1#5053";
#      REV_SERVER = "true";
#      REV_SERVER_TARGET = "10.0.0.1"; # Router IP.
#      REV_SERVER_CIDR = "10.0.0.0/16";
#      TZ = config.time.timeZone;
#      PROXY_LOCATION = "pihole";
#      # NOTE: This must agree with the nginx virtual host.
#      VIRTUAL_HOST = "pihole.${fqdn}";
#      # TODO: Change this to something secure, obviously.
#      WEBPASSWORD = "hunter2";
#    };
#    extraOptions = [ "--dns=127.0.0.1" "--dns=9.9.9.9" ];
#    workdir = "/etc/pihole";
#    autoStart = true;
#  };
#  systemd.services.podman-pihole = {
#    after = [ "network.target" ];
#    wantedBy = [ "multi-user.target" ];
#  };
#}
