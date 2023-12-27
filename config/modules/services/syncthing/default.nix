{ config, lib, pkgs, ... }:

let
  hostName = config.networking.hostName;
  otherMachineNames =
    lib.remove config.networking.hostName (
      builtins.filter (machine: machine != "yellowstone.cofree.coffee") (builtins.attrNames (builtins.readDir ../../../machines))
    );

  syncthingMachineIds = {
    sower = "QR6FWXC-CE7YTWC-D5FTHRL-R74OZ2L-RV4JIWP-TWKBQ7A-ARP637O-LFBPRQK";
    lorean = "BRI57JX-WTQMZJI-IN4PZJQ-QRXQLI2-USIIIYR-DAGJXMP-LDPDGDI-F7TOYQH";
    nightshade = "YWQU6WT-7FKGWGT-UXMNJRT-DONET7A-ZJZ2D6H-AOQEP5S-4ZXZSLL-Y2VDWQ6";
    phone = "3ORQWGG-OX63V27-B4OD2YV-IP54EQE-SUF6PQO-CVBIHBT-RI7DQPJ-2CGZ4AO";
  };
in
{
  boot.kernel.sysctl."fs.inotify.max_user_watches" = 1048576;

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = config.primary-user.name;
    dataDir = config.primary-user.home-manager.home.homeDirectory;
    cert = "/secrets/${hostName}-syncthing-cert";
    key = "/secrets/${hostName}-syncthing-key";
    devices = lib.genAttrs otherMachineNames (machine: {
      id = syncthingMachineIds."${machine}";
    });
    folders = {
      Org = {
        path = "${config.primary-user.home}/Org";
        devices = otherMachineNames;
      };
      Public = {
        path = "${config.primary-user.home}/Public";
        devices = otherMachineNames;
      };
    };
  };

  systemd.services = {
    syncthing = {
      wants = [
        "syncthing-cert-key.service"
        "syncthing-key-key.service"
      ];
      after = [
        "syncthing-cert-key.service"
        "syncthing-key-key.service"
      ];
    };

    syncthing-init.serviceConfig.ExecStartPost = pkgs.writeShellScript "rm-sync-dir" ''
      if [ -d "$HOME/Sync" ]
      then
        rmdir "$HOME/Sync"
      fi
    '';
  };
}
