{ pkgs, config, ... }:
{
  environment.systemPackages = [ pkgs.smbclient ];

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud24;
    hostName = "yellowstone.cofree.coffee";

    # Use HTTPS for links
    https = true;

    # Auto-update Nextcloud Apps
    autoUpdateApps.enable = true;
    # Set what time makes sense for you
    autoUpdateApps.startAt = "05:00:00";

    config = {
      # Further forces Nextcloud to use HTTPS
      overwriteProtocol = "https";

      # Nextcloud PostegreSQL database configuration, recommended over using SQLite
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      dbname = "nextcloud";
      dbpassFile = "/secrets/nextcloud-db-pass";
      adminpassFile = "/secrets/nextcloud-admin-pass";
      adminuser = "admin";
    };
  };

  users.users.nextcloud.extraGroups = [ "keys" ];

  services.postgresql = {
    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [
      { name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }
    ];
  };

  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };
}
