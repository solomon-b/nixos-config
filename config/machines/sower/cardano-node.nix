{ config, pkgs, ... }:

{
  config.services.cardano-node = {
    enable = true;
  };
}
