{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";

    home-manager = {
      url = "github:rycee/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kmonad = {
      url = "github:pnotequalnp/kmonad/flake?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmobar-solomon = {
      url = path:./flakes/xmobar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-solomon = {
      url = path:./flakes/xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
      self,
      nixpkgs,
      home-manager,
      kmonad,
      xmobar-solomon,
      xmonad-solomon
  }:
    let
      system = "x86_64-linux";
      #password-utils-overlay = import ./overlays/password-utils-overlay.nix;
      nix-plugins-overlay = import ./overlays/nix-plugins-overlay.nix;
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          kmonad.overlay
          xmobar-solomon.overlay
          xmonad-solomon.overlay
          nix-plugins-overlay
          #password-utils-overlay
        ];
      };
    in {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      inherit pkgs system;
      modules = [
        ./config/machines/laptop
        nixpkgs.nixosModules.notDetected
        home-manager.nixosModules.home-manager
      ];
    };
  };
}
