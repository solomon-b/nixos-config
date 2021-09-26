{ pkgs, ... }:

{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_12;
    settings = {
      shared_preload_libraries = "pg_stat_statements";
    };
  };
}
