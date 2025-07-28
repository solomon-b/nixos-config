{ pkgs, ... }:

{
  imports = [
    ./ipod-processing.nix
  ];

  services = {
    bazarr = {
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
    # (let src = builtins.readFile ./postprocess-ipod.sh;
    #      script = (pkgs.writeScriptBin "postproces-ipod" src).overrideAttrs (old: {
    #         buildCommand = "${old.buildCommand}\n patchShebangs $out";
    #      });
    #   in
    #   pkgs.symlinkJoin {
    #     name = "postprocess-ipod";
    #     paths = [ pkgs.ffmpeg pkgs.iconv script  ];
    #     buildInputs = [ pkgs.makeWrapper ];
    #     postBuild = "wrapProgram $out/bin/postprocess-ipod --prefix PATH : $out/bin";
    #   })
  ];

  # https://github.com/NixOS/nixpkgs/issues/155475#issuecomment-1093940244
  systemd.services.prowlarr.environment.HOME = "/var/empty";

  fileSystems."/mnt/media" = {
    device = "192.168.5.6:/mnt/tank/Media ";
    fsType = "nfs";
    options = [
      "defaults" # → rw,suid,dev,exec,auto,nouser,async
      "vers=3" # force NFSv3
      "proto=tcp" # use TCP transport
      "intr" # allow signals (Ctrl-C) to interrupt
      "timeo=30" # initial timeout = 3 s (30 deciseconds)
      "retrans=3" # retry only 3 times (~9 s total)
      "_netdev" # wait for network before mounting
    ];
  };

  services.nginx.virtualHosts = {
    "bazarr.service.home.arpa" = {
      locations."/" = {
        proxyPass = "http://localhost:6767";
      };
    };

    "prowlarr.service.home.arpa" = {
      locations."/" = {
        proxyPass = "http://localhost:9696";
      };
    };

    "radarr.service.home.arpa" = {
      locations."/" = {
        proxyPass = "http://localhost:7878";
      };
    };

    "sonarr.service.home.arpa" = {
      locations."/" = {
        proxyPass = "http://localhost:8989";
      };
    };
  };
}
