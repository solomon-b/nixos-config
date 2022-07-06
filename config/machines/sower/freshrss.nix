{ ... }:

{
  virtualisation.oci-containers.containers.freshrss = {
    image = "freshrss/freshrss";
    ports = [ "8081:80" ];
    volumes = [
      "/srv/www/freshrss/data:/var/www/FreshRSS/data"
      "/srv/www/freshrss/extensions:/var/www/FreshRSS/extensions"
    ];
    environment = {
      TZ = "America/Los_Angeles";
      CRON_MIN = "1, 31";
    };
  };
}
