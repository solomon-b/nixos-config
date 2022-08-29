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
      podcast-dl
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
        ];
      };
    in {
      devShell."${system}" = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.colmena ];
      };

      colmena = {
        meta.nixpkgs = pkgs;

        apollyon = {
          imports = [
            ./config/machines/virtual/apollyon
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };

        silence-under-snow = {
          imports = [
            ./config/machines/virtual/silence-under-snow
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };

        sower = {
          imports = [
            ./config/machines/physical/sower
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };
      };

      nixosConfigurations = {
        lorean = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/physical/lorean
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };

        nightshade = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/physical/nightshade
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };

        madonna-of-the-wasps = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./config/machines/virtual/madonna-of-the-wasps
            nixpkgs.nixosModules.notDetected
            home-manager.nixosModules.home-manager
          ];
        };
      };
    };
}
