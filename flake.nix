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

    #taffybar-solomon = {
    #  url = path:/home/solomon/Development/Nix/nixos-config/flakes/taffybar-solomon;
    #  inputs.nixpkgs.follows = "nixpkgs";
    #};

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

    #cardano-node = {
    #  url = github:input-output-hk/cardano-node;
    #};

    #cardano-wallet = {
    #  url = github:input-output-hk/cardano-wallet;
    #};
  };

  outputs = inputs@{
      self,
      nixpkgs,
      flake-utils,
      unstable,
      home-manager,
      kmonad,
      #taffybar-solomon,
      brightness-bar,
      volume-bar,
      xmobar-solomon,
      xmonad-solomon,
      graphqurl,
      podcast-dl,
      #cardano-node,
      #cardano-wallet
  }:
    let
      system = "x86_64-linux";
      #password-utils-overlay = import ./overlays/password-utils-overlay.nix;
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          brightness-bar.overlay
          #cardano-node.overlay
          #cardano-wallet.overlay
          graphqurl.overlay
          kmonad.overlay
          podcast-dl.overlay
          #taffybar-solomon.overlay
          volume-bar.overlay
          xmobar-solomon.overlay
          xmonad-solomon.overlay
          #password-utils-overlay
        ];
      };
    in {
      devShell."${system}" = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.nixops_unstable ];
      };

      nixopsConfigurations.default = {
        inherit nixpkgs;
        "yellowstone.cofree.coffee" = { config, ... }: {
          deployment = {
            targetHost = "yellowstone.cofree.coffee";
            targetUser = config.primary-user.name;
            sshOptions = [ "-A" ];
            provisionSSHKey = false;
          };

          imports = [
            ./config/machines/yellowstone.cofree.coffee
            (home-manager.nixosModules.home-manager)
          ];
        };
      };

      nixosConfigurations = {
        lorean = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/lorean
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
          specialArgs = {
            inherit inputs;
          };
        };

        nightshade = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/nightshade
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
          specialArgs = {
            inherit inputs;
          };
        };

        sower = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/sower
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
            #cardano-node.nixosModules.cardano-node
          ];
          specialArgs = {
            inherit inputs;
          };
        };
      };
    };
}
