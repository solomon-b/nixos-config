{config, ...}:

{
  sops.secrets = {
    immich-server-url-file = {};
    immich-server-username-file = {};
    immich-server-password-file = {};
  };
  immich-sdcard-sync = {
    enable = true;
    immichServerUrlFile = "${config.sops.secrets.immich-server-url-file.path}";
    immichUsernameFile = "${config.sops.secrets.immich-server-username-file.path}";
    immichPasswordFile = "${config.sops.secrets.immich-server-password-file.path}";
    sdCardSerials = "Generic-_SD_MMC_20120501030900000-0:0";
  };
}
