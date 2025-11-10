{ config, pkgs, ... }:

let
  notifyScript = pkgs.writeShellScript "ntfy-notify" ''
    ${pkgs.libnotify}/bin/notify-send "$title" "$message"
  '';
in
{
  home.packages = with pkgs; [ ntfy-sh ];

  systemd.user.services.ntfy-subscribe = {
    Unit = {
      Description = "Ntfy notification subscriber for homelab alerts";
      After = [ "network-online.target" "sops-nix.service" ];
    };
    Service = {
      Type = "simple";
      Environment = "DISPLAY=:0";
      ExecStart = ''${pkgs.bash}/bin/bash -c '${pkgs.ntfy-sh}/bin/ntfy subscribe https://ntfy.sh/$(cat ${config.sops.secrets.ntfy-topic.path}) ${notifyScript}' '';
      Restart = "on-failure";
      RestartSec = "10s";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  sops.secrets.ntfy-topic = { };
}
