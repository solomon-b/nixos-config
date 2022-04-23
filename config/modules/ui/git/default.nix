{ ... }:

{
  primary-user.home-manager.programs.git = {
    enable = true;
    userName = "solomon";
    userEmail = "ssbothwell@gmail.com";
    extraConfig = {
      safe = {
        directory = "/home/solomon/Development/Nix/nixos-config";   
      };
    };
  };
}
