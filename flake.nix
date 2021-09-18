{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:rycee/home-manager/master";
    kmonad = {
      url = "github:pnotequalnp/kmonad/flake?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmobar-solomon.url = "github:ssbothwell/xmobar-solomon/main";
    xmonad-solomon.url = path:./flakes/xmonad;
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs = { self, nixpkgs, home-manager, kmonad, xmobar-solomon, xmonad-solomon, xmonad, xmonad-contrib }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          kmonad.overlay
          xmobar-solomon.overlay
          xmonad-solomon.overlay
          xmonad.overlay
          xmonad-contrib.overlay
        ];
      };
    in {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      inherit pkgs system;
      modules = [
        ./configuration.nix
        nixpkgs.nixosModules.notDetected
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.solomon = import ./home.nix;
        }
      ];
    };
  };
}
