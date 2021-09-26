{ ... }:

{
  users.users.localtimed.group = "localtimed";
  users.groups.localtimed = {};
  services.localtime.enable = true;
  #time.timeZone = "America/Los_Angeles";
}
