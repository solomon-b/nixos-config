{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-25.11;
    unstable.url = github:nixos/nixpkgs;

    flake-utils = {
      url = github:numtide/flake-utils;
    };

    home-manager = {
      url = github:nix-community/home-manager/release-25.11;
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

    nixos-hardware = {
      url = github:NixOS/nixos-hardware/master;
    };

    brightness-bar = {
      url = github:solomon-b/brightness-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    volume-bar = {
      url = github:solomon-b/volume-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-solomon = {
      url = github:solomon-b/xmonad-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
      inputs.flake-utils.follows = "flake-utils";
    };

    immich-sd-card-sync = {
      url = github:solomon-b/Immich-SD-Card-Upload-Service;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    music-archiver = {
      url = github:solomon-b/music-archiver;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    worktrunk = {
      url = github:max-sixty/worktrunk;
    };

    micasa = {
      url = github:cpcloud/micasa;
    };

    nixos-anywhere = {
      url = github:numtide/nixos-anywhere;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , unstable
    , flake-utils
    , home-manager
    , disko
    , sops-nix
    , nixos-hardware
    , nixos-generators
    , brightness-bar
    , volume-bar
    , xmonad-solomon
    , immich-sd-card-sync
    , music-archiver
    , worktrunk
    , micasa
    , nixos-anywhere
    }:
    let
      system = "x86_64-linux";
      unstable-pkgs = import unstable { inherit system; };
      nixos-anywhere-pkg = nixos-anywhere.packages.${system}.default;
      pkgs = import nixpkgs {
        inherit system;

        config = {
          allowUnfree = true;
          permittedInsecurePackages = [
            "aspnetcore-runtime-6.0.36"
            "aspnetcore-runtime-wrapped-6.0.36"
            "dotnet-sdk-6.0.428"
            "dotnet-sdk-wrapped-6.0.428"
          ];
        };

        overlays = with inputs; [
          brightness-bar.overlays.default
          volume-bar.overlays.default
          xmonad-solomon.overlays.default
          xmonad-solomon.overlays.xmonad
          xmonad-solomon.overlays.xmonad-contrib
          #music-archiver.overlay
          # (final: prev: { eww = eww.packages.${final.system}.default; })
          (final: prev: { prowlarr = unstable-pkgs.prowlarr; })
          (final: prev: { worktrunk = worktrunk.packages.${final.system}.default; })
        ];
      };

      mkServer = targetHost: { config, ... }: {
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
          disko.nixosModules.disko
        ];

        sops = {
          defaultSopsFile = ./secrets.yaml;
          secrets.primary-user-password = { };
        };
      };
    in
    {
      formatter."${system}" = pkgs.nixpkgs-fmt;
      devShells."${system}".default = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.colmena pkgs.sops ];
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

        # NixOS-Anywhere provisioning script
        # nix run '.#install'
        install =
          let
            src = builtins.readFile ./installer/install.sh;

            script = (pkgs.writeScriptBin "install" src).overrideAttrs (old: {
              buildCommand = "${old.buildCommand}\n patchShebangs $out";
            });
          in
          pkgs.symlinkJoin {
            name = "install";
            paths = [ pkgs.gum pkgs.jq pkgs.pass pkgs.openssh pkgs.rsync pkgs.ssh-to-age pkgs.sops pkgs.yq-go nixos-anywhere-pkg script ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = "wrapProgram $out/bin/install --prefix PATH : $out/bin";
          };

        deploy = pkgs.writeShellScriptBin "deploy" ''
          servers=$(ls config/machines/servers | grep -vE '^(void-warren)$')
          machine=$(echo "$servers" | ${pkgs.gum}/bin/gum choose)
          echo "Deploying '$machine'"
          ${pkgs.colmena}/bin/colmena apply --on $machine
        '';

        deploy-all = pkgs.writeShellScriptBin "deploy-all" ''
          ${pkgs.colmena}/bin/colmena apply --on @server
        '';
      };

      apps.x86_64-linux = {
        deploy = flake-utils.lib.mkApp { drv = self.packages.${system}.deploy; };
        deploy-all = flake-utils.lib.mkApp { drv = self.packages.${system}.deploy-all; };
        default = self.apps.${system}.deploy;
      };

      homeConfigurations.nightshade = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./config/machines/personal-computers/nightshade/home.nix
          sops-nix.homeManagerModules.sops
          {
            sops = {
              defaultSopsFile = ./secrets.yaml;
              age.keyFile = "/home/solomon/.config/sops/age/keys.txt";
              defaultSymlinkPath = "/run/user/1000/secrets";
              defaultSecretsMountPoint = "/run/user/1000/secrets.d";
            };
          }
        ];
      };

      nixosConfigurations = {
        lorean = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/personal-computers/lorean
            ({ ... }: {
              sops = {
                defaultSopsFile = ./secrets.yaml;
                secrets.primary-user-password = { };
              };

              home-manager.sharedModules = [
                sops-nix.homeManagerModules.sops
                {
                  sops = {
                    defaultSopsFile = ./secrets.yaml;
                    age.keyFile = "/home/solomon/.config/sops/age/keys.txt";
                    defaultSymlinkPath = "/run/user/1000/secrets";
                    defaultSecretsMountPoint = "/run/user/1000/secrets.d";
                  };
                }
              ];
            })
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            disko.nixosModules.disko
            sops-nix.nixosModules.sops
            immich-sd-card-sync.nixosModules.immichSdCardSync
          ];

          specialArgs = { inherit inputs; };
        };

        voice-of-evening = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/personal-computers/voice-of-evening
            ({ ... }: {
              sops = {
                defaultSopsFile = ./secrets.yaml;
                secrets.primary-user-password = { };
              };

              home-manager.sharedModules = [
                sops-nix.homeManagerModules.sops
                {
                  sops = {
                    defaultSopsFile = ./secrets.yaml;
                    age.keyFile = "/home/solomon/.config/sops/age/keys.txt";
                    defaultSymlinkPath = "/run/user/1000/secrets";
                    defaultSecretsMountPoint = "/run/user/1000/secrets.d";
                  };
                }
              ];
            })
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
            disko.nixosModules.disko
          ];

          specialArgs = { inherit inputs; };
        };

        gnostic-ascension = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/servers/gnostic-ascension
            ({ ... }: {
              sops = {
                defaultSopsFile = ./secrets.yaml;
                secrets.primary-user-password = { };
              };
            })
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
            disko.nixosModules.disko
          ];

          specialArgs = { inherit inputs; };
        };

        sower = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/servers/sower
            ({ ... }: {
              sops = {
                defaultSopsFile = ./secrets.yaml;
                secrets.primary-user-password = { };
              };
            })
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
            disko.nixosModules.disko
          ];

          specialArgs = { inherit inputs; };
        };

        test-vm = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/servers/test-vm
            ({ ... }: {
              sops = {
                defaultSopsFile = ./secrets.yaml;
                secrets.primary-user-password = { };
              };
            })
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
            disko.nixosModules.disko
          ];

          specialArgs = { inherit inputs; };
        };

        # void-warren = nixpkgs.lib.nixosSystem {
        #   system = "aarch64-linux";
        #   pkgs = import nixpkgs {
        #     system = "aarch64-linux";
        #     config.allowUnfree = true;
        #   };
        #   modules = [
        #     ./config/machines/servers/void-warren
        #     ({ ... }: {
        #       sops = {
        #         defaultSopsFile = ./secrets.yaml;
        #         secrets.primary-user-password = { };
        #       };
        #     })
        #     nixpkgs.nixosModules.notDetected
        #     home-manager.nixosModules.home-manager
        #     sops-nix.nixosModules.sops
        #     disko.nixosModules.disko
        #   ];

        #   specialArgs = { inherit inputs; };
        # };
      };

      colmena =
        let
          configs = self.nixosConfigurations;
        in
        {
          meta = {
            nixpkgs = pkgs;
            specialArgs = {
              inherit inputs;
            };
            nodeNixpkgs = builtins.mapAttrs (name: value: value.pkgs) configs;
            nodeSpecialArgs = builtins.mapAttrs (name: value: value._module.specialArgs) configs;
          };
        } // builtins.mapAttrs (machine: _: mkServer machine)
          (builtins.removeAttrs (builtins.readDir ./config/machines/servers) [ "void-warren" "gnostic-ascension" ])
        // builtins.mapAttrs
          (name: value: {
            deployment = {
              targetHost = name;
              tags = [ "pc" ];
              allowLocalDeployment = true;
            };
            imports = value._module.args.modules;
          })
          configs;
    };
}
