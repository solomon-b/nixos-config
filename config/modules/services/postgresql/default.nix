{ pkgs, ... }:

{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_13;
    extraPlugins = [ pkgs.postgresql_13.pkgs.postgis ];
    settings = {
      shared_preload_libraries = "pg_stat_statements";
    };
  };
}
