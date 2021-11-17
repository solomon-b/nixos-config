{ config, ... }:
{
  imports = [
    ../../profiles/base
    ../../modules/system/timezone
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
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAA3aCawCBpgWlicoJl0IJh0GNAKypHOd3CCixMVKMXalZX/q1g36bFIyZqvLnBtC1VBYPc54LoLpkxfPqx384HjxnKo5i6VHs1YNPA//xwNCX1FejTbFc3a6k6hbz9mxvS4alNy+146BKNSeJmYsvKqlPcFbrHMMKSVjzna2yvmenUJ5zzYHDGYfxIIlX2gDchNHSiZCicgm4x+M8uSKe1sNz7t0TjZq2rE183Df5Ao4lami2331cMdY7CkZ/OsYSAsWtW/770EIZ2yIRlxfJCCEUCwfqqWZGP9VmIW83VnDPHzP+AIM5yb00UwALaIAm5nIbv+NEH+qlmBcXTkssGoJE2ddr6URs9qRehDehr6fNkiTmsZt7vMoifK3j20DzBCDn3+zBxdKFYxUkN+W5v2ANFDR4QNFalgeaMGjOmQ4Iv3AikvA+ayOyR2TALEMXICBjwD2VMo+GAdkhvEm3yba79dUaXV+uGIjQHDYTeV8kf9IiW/NIwmUpiUmf10Rl4VCTM4hR7HKuqNJ9OizK4LypB2qrUNBO9mCzHevxo/g0pz+XZ473ra1t0krlVzU/oKwgkBDTz0n1s6LeFyXo98PCAktFymynEy6aQpCaSXIZo1ZLW57ATQeUtb0q0txt9hmCFOjnxopMUv4G10D56I5V9Kfc/HPV0211zUPgj8R"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHedhPWMgsGFQS7niiFlgkCty/0yS68tVP0pm4x4PQLp"
  ];

}
