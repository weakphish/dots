{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      # Vi key bindings
      fish_vi_key_bindings

      # Bat theme
      set -Ux BAT_THEME base16

      # Initialize tools
      zoxide init fish | source
      fzf --fish | source
    '';

    shellAliases = {
      # Git
      lg = "lazygit";

      # Docker
      ldo = "lazydocker";
      dc = "docker compose";

      # DevOps
      pu = "pulumi";
      tf = "terraform";
      k = "kubectl";

      # Python
      p = "poetry run";

      # Better defaults
      ls = "lsd";
      la = "lsd -la";
      cat = "bat";
    };

    functions = {
      commit = {
        description = "Create a conventional commit with gum";
        body = ''
          set TYPE (gum choose "fix" "feat" "docs" "style" "refactor" "test" "chore" "revert")
          set SCOPE (gum input --placeholder "scope")
          set SUMMARY (gum input --value "$TYPE-$SCOPE: " --placeholder "Summary of this change")
          set DESCRIPTION (gum write --placeholder "Details of this change")
          gum confirm "Commit changes?" && git commit -m "$SUMMARY" -m "$DESCRIPTION"
        '';
      };
    };

    plugins = [
      # Add any fish plugins here
    ];
  };

  # Fish needs these programs available
  home.packages = with pkgs; [
    gum  # For commit function
  ];
}
