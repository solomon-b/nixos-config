# NOTE: Run the update script to initialize the database:
# sudo -u tt_rss /nix/store/0ch7n28h724dpvkc30z18jgy21s9p49b-php-with-extensions-8.0.22/bin/php /var/lib/tt-rss/www/update.php --update-schema
{ config, ... }:

{
  services.tt-rss = {
    enable = true;
    selfUrlPath = "http://tt-rss.service.home.arpa";
    database = {
      host = "transfigured-night";
      port = 5432;
      passwordFile = config.sops.secrets.tt_rss-password.path;
    };
    singleUserMode = true;
  };

  sops.secrets.tt_rss-password = { };
}
