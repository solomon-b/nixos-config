{ config, lib, pkgs, ... }:
let
  cfg = config.primary-user;
  hostname = config.networking.hostName;
  passwords = pkgs.callPackage ../../lib/passwords.nix { };
  androidModule = lib.types.submodule ({config, ...}: {
    options.wireguardPubKey = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Wireguard public key";
    };
  });
in
{
  options.primary-user.name = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = "The name of the primary user account.";
  };

  options.primary-user.wireguardPubKey = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = "Wireguard Public Key";
  };

  options.primary-user.android = lib.mkOption {
    type = lib.types.nullOr androidModule;
    default = null;
    description = "Android phone metadata";
  };

  imports = [
    (lib.mkAliasOptionModule [ "primary-user" "home-manager" ] [ "home-manager" "users" cfg.name ])
    (lib.mkAliasOptionModule [ "primary-user" "home" ] [ "users" "users" cfg.name "home" ])
    (lib.mkAliasOptionModule [ "primary-user" "shell" ] [ "users" "users" cfg.name "shell" ])
    (lib.mkAliasOptionModule [ "primary-user" "extraGroups" ] [ "users" "users" cfg.name "extraGroups" ])
    (lib.mkAliasOptionModule [ "primary-user" "uid" ] [ "users" "users" cfg.name "uid" ])
    (lib.mkAliasOptionModule [ "primary-user" "openssh" ] [ "users" "users" cfg.name "openssh" ])
    (lib.mkAliasOptionModule [ "primary-user" "isNormalUser" ] [ "users" "users" cfg.name "isNormalUser" ])
    (lib.mkAliasOptionModule [ "primary-user" "passwordFile" ] [ "users" "users" cfg.name "passwordFile" ])
  ];

  config = lib.mkIf (cfg.name != null) {
    deployment.keys.primary-user-password = {
      keyCommand = passwords.getHashedUserPassword "system/${hostname}/${cfg.name}/password";
      destDir = "/secrets";
    };

    deployment.keys.primary-user-wireguard-pubkey = {
      keyCommand = passwords.getFullPassword "system/${hostname}/wireguard/public-key";
      destDir = "/secrets";
    };

    primary-user = {
      extraGroups = [ "wheel" "users" "keys" "input" ];

      home-manager = {
        home.username = cfg.name;
        home.homeDirectory = "/home/${cfg.name}";
        home.stateVersion = "21.03";
      };

      isNormalUser = true;
      passwordFile = config.deployment.keys.primary-user-password.path;
      shell = pkgs.zsh;
      uid = lib.mkDefault 1000;

      wireguardPubKey = builtins.extraBuiltins.getFullPasswordValue pkgs "system/solomon/wireguard/public-key";
      android.wireguardPubKey = builtins.extraBuiltins.getFullPasswordValue pkgs "system/android/wireguard/public-key";
    };

    nix.trustedUsers = [ cfg.name ];
  };
}
