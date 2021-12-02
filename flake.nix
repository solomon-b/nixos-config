{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.11;
    unstable.url = github:nixos/nixpkgs;
    nixops-plugged.url  = github:lukebfox/nixops-plugged;

    home-manager = {
      url = github:rycee/home-manager/release-21.11;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kmonad = {
      url = github:pnotequalnp/kmonad/flake?dir=nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    brightness-bar = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/brightness-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    volume-bar = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/volume-bar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    taffybar-solomon = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/taffybar-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmobar-solomon = {
      #url = path:./flakes/xmobar-solomon;
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmobar-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-solomon = {
      #url = path:./flakes/xmonad-solomon;
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmonad-solomon;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad = {
      #url = github:xmonad/xmonad;
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmonad-solomon/xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-contrib = {
      #url = github:xmonad/xmonad-contrib;
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/xmonad-solomon/xmonad-contrib;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    graphqurl = {
      url = path:/home/solomon/Development/Nix/nixos-config/flakes/graphqurl;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{
      self,
      nixpkgs,
      nixops-plugged,
      unstable,
      home-manager,
      kmonad,
      taffybar-solomon,
      brightness-bar,
      volume-bar,
      xmobar-solomon,
      xmonad-solomon,
      xmonad,
      xmonad-contrib,
      graphqurl
  }:
    let
      system = "x86_64-linux";
      #password-utils-overlay = import ./overlays/password-utils-overlay.nix;
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          brightness-bar.overlay
          graphqurl.overlay
          kmonad.overlay
          taffybar-solomon.overlay
          volume-bar.overlay
          xmonad.overlay
          xmonad-contrib.overlay
          xmobar-solomon.overlay
          xmonad-solomon.overlay
          #password-utils-overlay
        ];
      };
    in {
      devShell."${system}" = pkgs.mkShell {
        nativeBuildInputs = [ nixops-plugged.defaultPackage.${system} ];
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
          ];
          specialArgs = {
            inherit inputs;
          };
        };
      };
    };
}
