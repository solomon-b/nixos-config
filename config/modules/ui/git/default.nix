{ ... }:

{
  primary-user.home-manager.programs.git = {
    enable = true;
    userName = "solomon";
    userEmail = "ssbothwell@gmail.com";
    aliases = {
      # list files which have changed since REVIEW_BASE
      # (REVIEW_BASE defaults to 'master' in zshrc)
      files = "!git diff --name-only $(git merge-base HEAD \"$REVIEW_BASE\")";

      # Same as above, but with a diff stat instead of just names
      # (better for interactive use)
      stat = "!git diff --stat $(git merge-base HEAD \"$REVIEW_BASE\")";
    };
    extraConfig = {
      branch = {
        sort = "-committerdate";
      };
      column = {
        ui = "auto";
      };
      gpg = {
        format = "ssh";
      };
      init = {
        defaultBranch = "main";
      };
      merge = {
        # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#merge-conflictstyle-zdiff3
        conflictstyle = "zdiff3";
      };
      pull = {
        # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#pull-ff-only-or-pull-rebase-true
        rebase = true;
      };
      push = {
        # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#push-default-simple-push-default-current
        autoSetupRemote = true;
      };
      # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#rerere-enabled-true
      rerere = {
        enabled = true;
      };
      safe = {
        directory = "/etc/nixos/flake";
      };
      user = {
        signingKey = "~/.ssh/id_ed25519.pub";
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
