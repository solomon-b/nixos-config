{ ... }:

{
  primary-user.home-manager.programs.git = {
    enable = true;
    userName = "solomon";
    userEmail = "ssbothwell@gmail.com";
    extraConfig = {
      safe = {
        directory = "/etc/nixos/flake";
      };
    };
    includes = [
      {
        path = ./work-profile;
        condition = "gitdir:~/Development/Co-Star/";
      }
    ];
  };
}
