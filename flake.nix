{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-23.05;
    unstable.url = github:nixos/nixpkgs;
    
    flake-utils = {
      url = github:numtide/flake-utils;
    };

    home-manager = {
      url = github:rycee/home-manager/release-23.05;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = github:nix-community/disko;
      inputs.nixpkgs.follows = "unstable";
    };

    sops-nix = {
      url = github:Mic92/sops-nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = github:nix-community/nixos-generators;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kmonad = {
      url = github:pnotequalnp/kmonad/flake?dir=nix;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    brightness-bar = {
      url = path:/etc/nixos/flake/flakes/brightness-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    volume-bar = {
      url = path:/etc/nixos/flake/flakes/volume-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmobar-solomon = {
      url = path:/etc/nixos/flake/flakes/xmobar-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    xmonad-solomon = {
      url = path:/etc/nixos/flake/flakes/xmonad-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
      inputs.flake-utils.follows = "flake-utils";
    };

    graphqurl = {
      url = path:/etc/nixos/flake/flakes/graphqurl;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fonts = {
      url = path:/etc/nixos/flake/flakes/fonts;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{
      self,
      nixpkgs,
      unstable,
      flake-utils,
      home-manager,
      disko,
      sops-nix,
      nixos-generators,
      kmonad,
      brightness-bar,
      volume-bar,
      xmobar-solomon,
      xmonad-solomon,
      graphqurl,
      fonts
  }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;

        config = {
          allowUnfree = true;
        };

        overlays = with inputs; [
          brightness-bar.overlay
          graphqurl.overlay
          kmonad.overlay
          volume-bar.overlay
          xmobar-solomon.overlay
          xmonad-solomon.overlays.default
          xmonad-solomon.overlays.xmonad
          xmonad-solomon.overlays.xmonad-contrib
          fonts.overlays.default
          (final: prev: { eww = eww.packages.${final.system}.default; })
          #(self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = true; }; } )
        ];
      };
      
      mkServer = targetHost: {config, ...}: {
        deployment = {
          inherit targetHost;
          #targetUser = config.primary-user.name;
          tags = [ "server" ];
          allowLocalDeployment = false;
        };
        
        imports = [
          "${toString config/machines/servers}/${targetHost}"
          nixpkgs.nixosModules.notDetected
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
        ];

        sops = {
          defaultSopsFile = ./secrets.yaml;
          secrets.primary-user-password = { };
        };
      };
    in {
      devShells."${system}".default = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.colmena pkgs.nixfmt ];
      };

      packages.x86_64-linux = {
        # NixOS boot disk with my SSH Keys integrated
        nixos-iso = nixos-generators.nixosGenerate {
          inherit system;
          format = "install-iso";
          modules = [
            ./installer
            home-manager.nixosModules.home-manager
          ];
        };

        # NixOS-Anywhere provisioning script for physical machines.
        # nix run '.#install-pc'
        install-pc =
          let src = builtins.readFile ./installer/install-pc.sh;

              script = (pkgs.writeScriptBin "install-pc" src).overrideAttrs(old: {
                buildCommand = "${old.buildCommand}\n patchShebangs $out";
              });
          in pkgs.symlinkJoin {
            name = "install-pc";
            paths = [ pkgs.gum pkgs.jq pkgs.pass script ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = "wrapProgram $out/bin/install-pc --prefix PATH : $out/bin";
          };
      };

      nixosConfigurations = {
        lorean = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/personal-computers/lorean
            ( { ... }: {
              sops = {
                defaultSopsFile = ./secrets.yaml;
                secrets.primary-user-password = { };
              };
            })
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            disko.nixosModules.disko
            sops-nix.nixosModules.sops
          ];

          specialArgs = { inherit inputs; };
        };

        nightshade = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/personal-computers/nightshade
            ( { ... }: {
              sops = {
                defaultSopsFile = ./secrets.yaml;
                secrets.primary-user-password = { };
              };
            })
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
          ];

          specialArgs = { inherit inputs; };
        };
      };

      colmena =
        let
          configs = self.nixosConfigurations;
        in {
          meta = {
            nixpkgs = pkgs;
            specialArgs = {
              inherit inputs;
            };
            nodeNixpkgs = builtins.mapAttrs (name: value: value.pkgs) configs;
            nodeSpecialArgs = builtins.mapAttrs (name: value: value._module.specialArgs) configs; 
          };
      } // builtins.mapAttrs (machine: _: mkServer machine) (builtins.readDir ./config/machines/servers)
        // builtins.mapAttrs (name: value: {
          deployment = {
            targetHost = name;
            tags = [ "pc" ];
            allowLocalDeployment = true;
          };
          imports = value._module.args.modules;
        }) configs;
    };
}
