{ ... }:

{
  # Advertise the local subnet to Tailscale for subnet routing
  systemd.services.tailscale-subnet-router = {
    description = "Tailscale Subnet Router Configuration";
    after = [ "tailscaled.service" ];
    wants = [ "tailscaled.service" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      # Wait for tailscaled to be ready
      sleep 2

      # Advertise the 192.168.5.0/24 subnet
      /run/current-system/sw/bin/tailscale up --advertise-routes=192.168.5.0/24 --accept-routes
    '';
  };
}
