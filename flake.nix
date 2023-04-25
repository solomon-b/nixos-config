{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-22.11;

    nixos-generators = {
      url = github:nix-community/nixos-generators;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    flake-utils = {
      url = github:numtide/flake-utils;
    };

    home-manager = {
      url = github:rycee/home-manager/release-22.05;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
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
      url = path:./flakes/xmonad-solomon;
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

    fonts = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/fonts;
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
      nixos-generators,
      unstable,
      flake-utils,
      sops-nix,
      home-manager,
      kmonad,
      brightness-bar,
      volume-bar,
      xmobar-solomon,
      xmonad-solomon,
      graphqurl,
      podcast-dl,
      gum,
      fonts
  }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      
      mkMachine = tag: path: local: targetHost: {config, ...}: {
        deployment = {
          inherit targetHost;
          #targetUser = config.primary-user.name;
          tags = [ tag ];
          allowLocalDeployment = local;
        };
        
        imports = [
          "${toString path}/${targetHost}"
          nixpkgs.nixosModules.notDetected
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
        ];

        sops = {
          defaultSopsFile = ./secrets.yaml;
          secrets.primary-user-password = { };
        };
      };

      mkServer = mkMachine "server" ./config/machines/servers false;
      mkPersonalComputer = mkMachine "pc" ./config/machines/personal-computers true;
    in {
      devShell."${system}" = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.colmena pkgs.nixfmt ];
      };

      packages."${system}".storm-bird-installer = nixos-generators.nixosGenerate {
        inherit system;
        modules = [
          ./configuration.nix
        ];
        format = "iso";
      };

      colmena = {
        meta.nixpkgs = pkgs;
        meta.specialArgs = {
          inherit inputs;
        };
      } // builtins.mapAttrs (machine: _: mkServer machine) (builtins.readDir ./config/machines/servers)
        // builtins.mapAttrs (machine: _: mkPersonalComputer machine) (builtins.readDir ./config/machines/personal-computers);
    };
}
