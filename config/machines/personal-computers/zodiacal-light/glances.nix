{ pkgs, ... } :

{
  systemd.services.glances-service = {
    enable = true;
    after = [ "network.target" ];
    wantedBy = [ "default.target" ];
    description = "Glances Service";
    serviceConfig = {
        Type = "simple";
        ExecStart = ''
          ${pkgs.glances}/bin/glances -s
        '';
    };
  };
}
