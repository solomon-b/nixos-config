# NOTE: Run the update script to initialize the database:
# sudo -u tt_rss /nix/store/0ch7n28h724dpvkc30z18jgy21s9p49b-php-with-extensions-8.0.22/bin/php /var/lib/tt-rss/www/update.php --update-schema
{ ... }:

{
  services.tt-rss = {
    enable = true;
    selfUrlPath = "http://tt-rss.service";
    database = {
      host = "transfigured-night";
      port = 5432;
      #TODO: Figure out why the passwordFile option isn't working or
      #use agenix for secrets managment.
      password = "password";
      #passwordFile = "/secrets/tt_rss-postgres-password";
    };
    singleUserMode = true;
  };
}
