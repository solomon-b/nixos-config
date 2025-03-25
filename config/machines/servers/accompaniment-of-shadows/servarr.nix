{ pkgs, ... }:

{
  services = {
    bazarr = {
      enable = true;
      openFirewall = true;
    };

    lidarr = {
      enable = true;
      openFirewall = true;
    };

    prowlarr = {
      enable = true;
      openFirewall = true;
    };


    radarr = {
      enable = true;
      openFirewall = true;
    };

    sonarr = {
      enable = true;
      openFirewall = true;
    };
  };

  environment.systemPackages = [
      pkgs.ffmpeg
      (let src = builtins.readFile ./postprocess-ipod.sh;
           script = (pkgs.writeScriptBin "postproces-ipod" src).overrideAttrs (old: {
              buildCommand = "${old.buildCommand}\n patchShebangs $out";
           });
        in
        pkgs.symlinkJoin {
          name = "postproces-ipod";
          paths = [ pkgs.ffmpeg pkgs.iconv script  ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = "wrapProgram $out/bin/postprocess-ipod --prefix PATH : $out/bin";
        })
    ];

  # https://github.com/NixOS/nixpkgs/issues/155475#issuecomment-1093940244
  systemd.services.prowlarr.environment.HOME = "/var/empty";

  fileSystems."/mnt/media" = {
    device = "192.168.5.6:/mnt/tank/Media ";
    fsType = "nfs";
  };

  services.nginx.virtualHosts = {
    "bazarr.service" = {
      locations."/" = {
        proxyPass = "http://localhost:6767";
      };
    };

    "lidarr.service" = {
      locations."/" = {
        proxyPass = "http://localhost:8686";
      };
    };

    "prowlarr.service" = {
      locations."/" = {
        proxyPass = "http://localhost:9696";
      };
    };

    "radarr.service" = {
      locations."/" = {
        proxyPass = "http://localhost:7878";
      };
    };

    "sonarr.service" = {
      locations."/" = {
        proxyPass = "http://localhost:8989";
      };
    };
  };
}
