{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.05;
    #nixops-plugged.url  = github:lukebfox/nixops-plugged;

    home-manager = {
      url = github:rycee/home-manager/release-21.05;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kmonad = {
      url = github:pnotequalnp/kmonad/flake?dir=nix;
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
  };

  outputs = {
      self,
      nixpkgs,
      home-manager,
      kmonad,
      xmobar-solomon,
      xmonad-solomon,
      xmonad,
      xmonad-contrib
  }:
    let
      system = "x86_64-linux";
      #password-utils-overlay = import ./overlays/password-utils-overlay.nix;
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          kmonad.overlay
          xmonad.overlay
          xmonad-contrib.overlay
          xmobar-solomon.overlay
          xmonad-solomon.overlay
          #password-utils-overlay
        ];
      };
    in {
      #devShell."system" = pkgs.mkShell {
      #  buildInputs = [ pkgs.nixops ];
      #};

      nixosConfigurations = {
        #yellowstone.cofree.coffee = nixpkgs.lib.nixosSystem {
        #  inherit pkgs system;
        #  modules = [
        #    ./config/machines/yellowstone.cofree.coffee
        #    nixpkgs.nixosModules.notDetected
        #    home-manager.nixosModules.home-manager
        #  ];
        #};

        nixos = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/laptop
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };

        nightshade = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/nightshade
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };

        #sower = nixpkgs.lib.nixosSystem {
        #  inherit pkgs system;
        #  modules = [
        #    ./config/machines/sower
        #    nixpkgs.nixosModules.notDetected
        #    home-manager.nixosModules.home-manager
        #  ];
        #};
      };
  };
}
