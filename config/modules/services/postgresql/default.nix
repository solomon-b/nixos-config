{ pkgs, ... }:

{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql;
    extraPlugins = [ pkgs.postgresql.pkgs.postgis ];
    settings = {
      shared_preload_libraries = "pg_stat_statements";
    };
  };
}
