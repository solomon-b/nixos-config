{ config, pkgs, ... }:

{
  config.services.cardano-node = {
    enable = true;
  };

  environment.systemPackages = [
    pkgs.cardano-node
    pkgs.cardano-cli
    pkgs.cardano-wallet
    pkgs.tx-generator
  ];
}
