{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-26.05;
    unstable.url = github:nixos/nixpkgs;

    flake-utils = {
      url = github:numtide/flake-utils;
    };

    home-manager = {
      url = github:nix-community/home-manager/release-26.05;
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

    claude-code = {
      url = github:sadjow/claude-code-nix;
    };

    friendly-ghost = {
      url = github:solomon-b/friendly-ghost;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-anywhere = {
      url = github:numtide/nixos-anywhere;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = github:nix-darwin/nix-darwin/nix-darwin-25.11;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-doom-emacs-unstraightened = {
      url = github:marienz/nix-doom-emacs-unstraightened;
      inputs.nixpkgs.follows = "";
    };

    hermes-agent = {
      url = github:NousResearch/hermes-agent;
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
    , nixos-generators
    , brightness-bar
    , volume-bar
    , xmonad-solomon
    , immich-sd-card-sync
    , music-archiver
    , worktrunk
    , micasa
    , claude-code
    , friendly-ghost
    , nixos-anywhere
    , nix-darwin
    , nix-doom-emacs-unstraightened
    , hermes-agent
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
          claude-code.overlays.default
          (final: prev: { worktrunk = worktrunk.packages.${final.system}.default; })
        ];
      };

      # Apple Silicon work machine. Kept separate from the Linux `pkgs` above
      # because the xmonad/brightness-bar/etc. overlays are Linux-only.
      darwinSystem = "aarch64-darwin";
      darwinPkgs = import nixpkgs {
        system = darwinSystem;
        config.allowUnfree = true;
        overlays = [
          claude-code.overlays.default
          (final: prev: { worktrunk = worktrunk.packages.${final.system}.default; })
        ];
      };

      # Every directory under config/machines/servers is a server.
      serverNames = builtins.attrNames (nixpkgs.lib.filterAttrs
        (name: kind: kind == "directory")
        (builtins.readDir ./config/machines/servers));

      serverModules = name: [
        ./config/machines/servers/${name}
        nixpkgs.nixosModules.notDetected
        home-manager.nixosModules.home-manager
        sops-nix.nixosModules.sops
        disko.nixosModules.disko
        friendly-ghost.nixosModules.default
        ({ ... }: {
          # A host can set its own default sops file.
          sops = {
            defaultSopsFile = nixpkgs.lib.mkDefault ./secrets.yaml;
            secrets.primary-user-password = { };
          };
        })
      ];

      mkServer = name: {
        deployment = {
          targetHost = name;
          tags = [ "server" ];
          allowLocalDeployment = false;
        };

        imports = serverModules name;
      };

      mkServerSystem = name: nixpkgs.lib.nixosSystem {
        inherit pkgs system;
        modules = serverModules name;
        specialArgs = { inherit inputs; };
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
            paths = [ pkgs.curl pkgs.gum pkgs.jq pkgs.pass pkgs.openssh pkgs.rsync pkgs.ssh-to-age pkgs.sops pkgs.yq-go nixos-anywhere-pkg script ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = "wrapProgram $out/bin/install --prefix PATH : $out/bin --set INSTALLER_LIB_DIR ${./installer/lib}";
          };

        deploy = pkgs.writeShellScriptBin "deploy" ''
          servers=$(ls config/machines/servers)
          machine=$(echo "$servers" | ${pkgs.gum}/bin/gum choose)
          echo "Deploying '$machine'"
          ${pkgs.colmena}/bin/colmena apply --on $machine
        '';

        deploy-all = pkgs.writeShellScriptBin "deploy-all" ''
          ${pkgs.colmena}/bin/colmena apply --on @server
        '';

        check-pins = pkgs.writeShellApplication {
          name = "check-pins";
          runtimeInputs = with pkgs; [ skopeo gum jq gnused gawk gnugrep coreutils git ];
          text = builtins.readFile ./tools/check-pins/check-pins.sh;
        };

        irrigation = pkgs.buildGoModule {
          pname = "irrigation";
          version = "0.1.0";
          src = ./tools/irrigation;
          vendorHash = "sha256-qgvqRl8KodAYkIfsBQSoM7MBACZnDZj9LgKhIyJEFq0=";
          meta.mainProgram = "irrigation";
        };
      };

      apps.x86_64-linux = {
        deploy = flake-utils.lib.mkApp { drv = self.packages.${system}.deploy; };
        deploy-all = flake-utils.lib.mkApp { drv = self.packages.${system}.deploy-all; };
        check-pins = flake-utils.lib.mkApp { drv = self.packages.${system}.check-pins; };
        irrigation = flake-utils.lib.mkApp { drv = self.packages.${system}.irrigation; };
        default = self.apps.${system}.deploy;
      };

      homeConfigurations.nightshade = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = { inherit inputs; };
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

      # Work MacBook (Apple Silicon). Apply with:
      #   darwin-rebuild switch --flake .#work-macbook
      darwinConfigurations.work-macbook = nix-darwin.lib.darwinSystem {
        pkgs = darwinPkgs;
        modules = [
          ./config/machines/personal-computers/work-macbook
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.solomon =
              import ./config/machines/personal-computers/work-macbook/home.nix;
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
      } // nixpkgs.lib.genAttrs serverNames mkServerSystem;

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
        } // nixpkgs.lib.genAttrs serverNames mkServer
        // builtins.mapAttrs
          (name: value: {
            deployment = {
              targetHost = name;
              tags = [ "pc" ];
              allowLocalDeployment = true;
            };
            imports = value._module.args.modules;
          })
          (builtins.removeAttrs configs serverNames);
    };
}
