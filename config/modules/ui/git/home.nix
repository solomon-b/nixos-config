{ ... }:

{
  programs.git = {
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

      # https://blog.dbrgn.ch/2021/11/16/git-ssh-signatures/
      commit = {
        gpgsign = true;
      };

      core = {
        fscache = false;
      };

      gpg = {
        format = "ssh";
        ssh = {
          allowedSignersFile = "~/.config/git/allowed_signers";
        };
      };

      init = {
        defaultBranch = "main";
      };

      # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#merge-conflictstyle-zdiff3
      merge = {
        conflictstyle = "zdiff3";
      };

      # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#pull-ff-only-or-pull-rebase-true
      pull = {
        rebase = true;
      };

      # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#push-default-simple-push-default-current
      push = {
        autoSetupRemote = true;
      };

      # https://jvns.ca/blog/2024/02/16/popular-git-config-options/#rerere-enabled-true
      rerere = {
        enabled = true;
      };

      safe = {
        directory = "/etc/nixos/flake";
      };

      # https://blog.dbrgn.ch/2021/11/16/git-ssh-signatures/
      tag = {
        gpgsign = true;
      };

      user = {
        email = "ssbothwell@gmail.com";
        signingKey = "~/.ssh/id_ed25519.pub";
      };
    };
    includes = [
      {
        path = ./work-profile;
        condition = "gitdir:~/Development/Bitnomial/";
      }
    ];
  };
}
