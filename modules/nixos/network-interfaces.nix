{ lib, ... }:

{
  options.interfaces = lib.mkOption {
    description = "The list of networking interface names.";

    type = lib.types.submodule {
      options = {
        wifi = lib.mkOption {
          default = null;
          type = lib.types.nullOr lib.types.str;
          description = "The wifi interface.";
        };
        eth = lib.mkOption {
          default = null;
          type = lib.types.nullOr lib.types.str;
          description = "The ethernet interface.";
        };
      };
    };
  };
}
