{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;

    # Configure your user info
    # userName = "Your Name";
    # userEmail = "your.email@example.com";

    delta = {
      enable = true;
      options = {
        navigate = true;
        light = false;
        side-by-side = true;
        line-numbers = true;
      };
    };

    extraConfig = {
      init.defaultBranch = "main";
      push.autoSetupRemote = true;
      pull.rebase = true;
      merge.conflictStyle = "diff3";
      diff.colorMoved = "default";

      core = {
        editor = "nvim";
      };
    };

    aliases = {
      co = "checkout";
      ci = "commit";
      st = "status";
      br = "branch";
      lg = "log --oneline --graph --decorate";
    };
  };
}
