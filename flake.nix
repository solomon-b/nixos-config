{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-22.05;
    
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = github:rycee/home-manager/release-22.05;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kmonad = {
      url = github:pnotequalnp/kmonad/flake?dir=nix;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    brightness-bar = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/brightness-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    volume-bar = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/volume-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmobar-solomon = {
      #url = path:./flakes/xmobar-solomon;
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmobar-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    xmonad-solomon = {
      #url = path:./flakes/xmonad-solomon;
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmonad-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
      inputs.flake-utils.follows = "flake-utils";
    };

    graphqurl = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/graphqurl;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    podcast-dl = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/podcast-dl;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gum = {
      url = github:solomon-b/gum;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{
      self,
      nixpkgs,
      flake-utils,
      unstable,
      home-manager,
      kmonad,
      brightness-bar,
      volume-bar,
      xmobar-solomon,
      xmonad-solomon,
      graphqurl,
      podcast-dl,
      gum
  }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          brightness-bar.overlay
          graphqurl.overlay
          kmonad.overlay
          podcast-dl.overlay
          volume-bar.overlay
          xmobar-solomon.overlay
          xmonad-solomon.overlay
          gum.overlays.default
        ];
      };

      mkServer = targetHost: {config, ...}: {
        imports = [
          "${toString ./config/machines/servers}/${targetHost}"
          nixpkgs.nixosModules.notDetected
          home-manager.nixosModules.home-manager
        ];
      };

      mkPersonalComputer = targetHost: 
        nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            "${toString ./config/machines/personal-computers}/${targetHost}"
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };
    in {
      devShell."${system}" = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.colmena ];
      };

      colmena = {
        meta.nixpkgs = pkgs;
      } // builtins.mapAttrs (machine: _: mkServer machine) (builtins.readDir ./config/machines/servers);

      nixosConfigurations = builtins.mapAttrs (machine: _: mkPersonalComputer machine) (builtins.readDir ./config/machines/personal-computers);
    };
}
