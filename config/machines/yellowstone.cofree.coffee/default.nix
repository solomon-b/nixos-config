{ config, ... }:
{
  imports = [
    ../../profiles/base
    ../../modules/system/zone/pacific
    ./hardware.nix
    ./letsencrypt.nix
    ./networking.nix
    ./nextcloud.nix
    ./nginx.nix
    ./postgres.nix
    ./wireguard.nix
  ];

  boot.cleanTmpDir = true;

  primary-user.name = "solomon";

  #programs.s3fs.enable = true;

  networking = {
    firewall.allowPing = true;
    hostName = "yellowstone";
    hostId = "930455f8";
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDEYye0R6efbTryJbYyZWdecbzxzlEUJmku7IlVA5zPhhk4yjkDcyrXwBJK1/QmT2ZUNMTGKgk4rcsIi/1nVaHyRKy7vNKMsHbVKziFyMrOAi2ovGnlfAfOmEK0TfUISXei6UoBCWnGsTxrbsv9ukOoTmaDp5oRRoXGf2Q7OI/oO4bsZSpS0JHbRVEKqaZzSSVrCn4FJ9vE2HFM1URRiChsBzM9JCSwjW0HgtJgYy4yLtw5L9PJSFNQJpw9X3yo4/VU96UFccmXS/IWBD+9704FfSWWcfdMK0bDZll0JXSTlUFvpHoZWO57Bm6T/Towx8JVCSo82tgHLW0vPvbHPEFT solomon@solomons-MBP.att.net"
  ];

}
