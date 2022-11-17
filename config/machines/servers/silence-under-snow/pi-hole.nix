{ config, pkgs, ... }:

# Run the pihole container on a bridged network and expose it over the
# tailscale mesh.
#
# https://shotor.com/blog/run-your-own-mesh-vpn-and-dns-with-tailscale-and-pihole/
# https://www.breakds.org/post/declarative-docker-in-nixos/
# https://github.com/jkachmar/dotnix/blob/trunk/config/services/dns/pihole.nix
{
  services.nginx.virtualHosts."pi.hole" = {
    locations."/" = {
      proxyPass = "http://172.21.0.2";
      extraConfig =
        "proxy_set_header Host $host;" +
        "proxy_set_header X-Real-IP $remote_addr;" +
        "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };
  };

  systemd.services.init-pihole-bridge = {
    description = "Create the network bridge for pihole.";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    
    serviceConfig.Type = "oneshot";
     script = let dockercli = "${config.virtualisation.docker.package}/bin/docker";
             in ''
               # Put a true at the end to prevent getting non-zero return code, which will
               # crash the whole service.
               check=$(${dockercli} network ls | grep "pihole-br" || true)
               if [ -z "$check" ]; then
                 ${dockercli} network create \
                   --driver=bridge \
                   --subnet=172.21.0.0/24 \
                   --gateway=172.21.0.1 \
                   -o "com.docker.network.bridge.enable_ip_masquerade"="true" \
                   -o "com.docker.network.bridge.enable_icc"="true" \
                   -o "com.docker.network.driver.mtu"="1500" \
                   pihole-br 
               else
                 echo "pihole-br already exists in docker"
               fi
             '';
  };

  virtualisation.oci-containers.containers.pihole = {
    image = "pihole/pihole:latest";
    volumes = [
      "/etc/pihole:/etc/pihole/"
      "/etc/dnsmasq.d:/etc/dnsmasq.d/"
    ];
    environment = {
      TZ = config.time.timeZone;
      VIRTUAL_HOST = "pi.hole";
      ServerIP="172.21.0.2";
      WEBPASSWORD = "hunter2";
    };

    extraOptions = [
      "--network=pihole-br"
      "--ip=172.21.0.2"
      "--dns=127.0.0.1"
      "--dns=1.1.1.1"
    ];
    workdir = "/etc/pihole";
    autoStart = true;
  };

  systemd.services.docker-pihole = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    postStart = ''
      sleep 10s
      docker exec pihole pihole -a addcustomdns 100.80.98.4 sower
      docker exec pihole pihole -a addcustomdns 100.117.45.47 silence-under-snow
      docker exec pihole pihole -a addcustomdns 100.92.19.49 lorean
      docker exec pihole pihole -a addcustomdns 100.70.16.79 madonna-of-the-wasps
      docker exec pihole pihole -a addcustomdns 100.80.98.4 jellyfin.sower
      docker exec pihole pihole -a addcustomdns 100.123.147.26 accompaniment-of-shadows
      docker exec pihole pihole -a addcustomdns 100.123.147.26 heimdall.local
      docker exec pihole pihole -a addcustomdns 100.123.147.26 filebrowser.sandra-voi.local
      docker exec pihole pihole -a addcustomdns 100.123.147.26 qbittorrent.local
      docker exec pihole pihole -a addcustomdns 100.123.147.26 tt-rss
      docker exec pihole pihole -a addcustomdns 100.96.251.72 transfigured-night
      docker exec pihole pihole -a addcustomdns 100.97.232.9 storm-bird
      docker exec pihole pihole -a addcustomdns 192.168.5.104 apollyon
      docker exec pihole pihole -a addcustomdns 192.168.5.6 sandra-voi.local
      docker exec pihole pihole -a addcustomdns 192.168.5.6 sandra-voi
      docker exec pihole pihole -a addcustomdns 100.100.33.33 nightshade
      docker exec pihole pihole -a addcustomdns 100.80.98.4 navidrome.sower
    '';
  };
}
