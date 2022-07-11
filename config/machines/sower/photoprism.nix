{ pkgs, ... }:

{
  systemd.services.photoprism-network = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${pkgs.docker}/bin/docker network create --driver bridge photoprism ||:
    '';
    after = [ "docker.service" ];
    before = [
      "docker-photoprism.service"
      "docker-mysql-photoprism.service"
    ];
  };

  virtualisation.oci-containers.containers.photoprism = {
    image = "photoprism/photoprism:latest";
    dependsOn = [ "mariadb" ];
    ports = [ "2342:2342" ];
    volumes = [
      "/srv/NAS/Media/Camera/Camera:/photoprism/originals"
    ];

    workdir = "/photoprism";

    extraOptions = [
      "--security-opt" "seccomp=unconfined"
      "--security-opt" "apparmor=unconfined"
      "--network=photoprism"
    ];

    environment = {
      PHOTOPRISM_ADMIN_PASSWORD = "insecure";          # YOUR INITIAL ADMIN PASSWORD (MINIMUM 8 CHARACTERS, USERNAME "admin")
      PHOTOPRISM_AUTH_MODE = "passwd";                 # authentication mode (public, passwd)
      PHOTOPRISM_SITE_URL = "http://localhost:2342/";  # public server URL incl http:// or https:// and /path, :port is optional
      PHOTOPRISM_ORIGINALS_LIMIT = "5000";             # file size limit for originals in MB (increase for high-res video)
      PHOTOPRISM_HTTP_COMPRESSION = "gzip";            # improves transfer speed and bandwidth utilization (none or gzip)
      PHOTOPRISM_LOG_LEVEL = "info";                   # log level: trace, debug, info, warning, error, fatal, or panic
      PHOTOPRISM_READONLY = "false";                   # do not modify originals directory (reduced functionality)
      PHOTOPRISM_EXPERIMENTAL = "false";               # enables experimental features
      PHOTOPRISM_DISABLE_CHOWN = "false";              # disables storage permission updates on startup
      PHOTOPRISM_DISABLE_WEBDAV = "false";             # disables built-in WebDAV server
      PHOTOPRISM_DISABLE_SETTINGS = "false";           # disables settings UI and API
      PHOTOPRISM_DISABLE_TENSORFLOW = "false";         # disables all features depending on TensorFlow
      PHOTOPRISM_DISABLE_FACES = "false";              # disables facial recognition
      PHOTOPRISM_DISABLE_CLASSIFICATION = "false";     # disables image classification
      PHOTOPRISM_DISABLE_RAW = "false";                # disables indexing and conversion of RAW files
      PHOTOPRISM_RAW_PRESETS = "false";                # enables applying user presets when converting RAW files (reduces performance)
      PHOTOPRISM_JPEG_QUALITY = "85";                  # image quality, a higher value reduces compression (25-100)
      PHOTOPRISM_DETECT_NSFW = "false";                # flag photos as private that MAY be offensive (requires TensorFlow)
      PHOTOPRISM_UPLOAD_NSFW = "true";                 # allows uploads that MAY be offensive
      PHOTOPRISM_DATABASE_DRIVER = "mysql";            # use MariaDB 10.5+ or MySQL 8+ instead of SQLite for improved performance
      PHOTOPRISM_DATABASE_SERVER = "mariadb:3306";     # MariaDB or MySQL database server (hostname:port)
      PHOTOPRISM_DATABASE_NAME = "photoprism";         # MariaDB or MySQL database schema name
      PHOTOPRISM_DATABASE_USER = "photoprism";         # MariaDB or MySQL database user name
      PHOTOPRISM_DATABASE_PASSWORD = "insecure";       # MariaDB or MySQL database user password
      PHOTOPRISM_SITE_CAPTION = "AI-Powered Photos App";
      PHOTOPRISM_SITE_DESCRIPTION = "";                # meta site description
      PHOTOPRISM_SITE_AUTHOR = "";                     # meta site author
    };
  };
  
  virtualisation.oci-containers.containers.mariadb = {
    image = "mariadb:10.8";
    extraOptions = [
      "--security-opt" "seccomp=unconfined"
      "--security-opt" "apparmor=unconfined"
      "--network=photoprism"
    ];
    ports = [ "3306:3306" ]; # no need to expose the database

    cmd = [
      "--innodb-buffer-pool-size=512M"
      "--transaction-isolation=READ-COMMITTED"
      "--character-set-server=utf8mb4"
      "--collation-server=utf8mb4_unicode_ci"
      "--max-connections=512"
      "--innodb-rollback-on-timeout=OFF"
      "--innodb-lock-wait-timeout=120"
    ];

    volumes = [
      "/etc/mysql:/var/lib/mysql" 
    ];
    environment = {
      MARIADB_AUTO_UPGRADE = "1";
      MARIADB_INITDB_SKIP_TZINFO = "1";
      MARIADB_DATABASE = "photoprism";
      MARIADB_USER = "photoprism";
      MARIADB_PASSWORD = "insecure";
      MARIADB_ROOT_PASSWORD = "insecure";
    };
  };

  services.nginx.virtualHosts."photos.sower" = {
    locations."/".proxyPass = "http://localhost:2342";
  };
}
