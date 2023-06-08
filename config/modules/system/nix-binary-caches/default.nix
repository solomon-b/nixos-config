{ ... }:

{
  nix.settings = {
    substituters = [
      "https://cache.nixos.org/"
      "https://nix-linter.cachix.org"
      "https://hydra.iohk.io"
      "https://iohk.cachix.org"
    ];
    trusted-public-keys = [
      "nix-linter.cachix.org-1:BdTne5LEHQfIoJh4RsoVdgvqfObpyHO5L0SCjXFShlE="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
    ];
  };
}
