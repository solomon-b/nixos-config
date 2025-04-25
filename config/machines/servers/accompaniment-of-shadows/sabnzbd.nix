{ config, ... }:

{
  services.sabnzbd = {
    enable = true;
    #group = "downloads";
  };

  ## TODO: Factor this out, since other downloads services depend on this group.
  ##
  ## Create the group for downloads.
  #users.groups.downloads.gid = 1010;

  services.nginx.virtualHosts = {
    "sabnzbd.service.home.arpa" = {
      locations."/" = {
        proxyPass = "http://localhost:8080";
      };
    };
  };
}
