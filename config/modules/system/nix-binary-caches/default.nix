{ ... }:

{
  nix = {
    binaryCaches = [
      "https://nix-linter.cachix.org"
      "https://hydra.iohk.io"
    ];
    binaryCachePublicKeys = [
      "nix-linter.cachix.org-1:BdTne5LEHQfIoJh4RsoVdgvqfObpyHO5L0SCjXFShlE="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
