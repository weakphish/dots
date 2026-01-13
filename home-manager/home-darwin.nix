{ config, pkgs, inputs, ... }:

{
  imports = [
    ./modules/fish.nix
    ./modules/starship.nix
    ./modules/git.nix
    ./modules/neovim.nix
    ./modules/helix.nix
    ./modules/ghostty.nix
  ];

  home = {
    username = "weakphish";
    homeDirectory = "/Users/weakphish";
    stateVersion = "24.11";

    packages = with pkgs; [
      # CLI utilities
      bat
      delta
      lsd
      ripgrep
      fd
      fzf
      zoxide
      lazygit
      lazydocker
      gum
      jq
      yq

      # Development tools
      uv
      nodejs
      go
      rustup

      # Cloud/DevOps
      kubectl
      k9s
      pulumi
      terraform
      docker-compose

      # Fonts
      (nerdfonts.override { fonts = [ "JetBrainsMono" "FiraCode" ]; })
    ];

    sessionVariables = {
      EDITOR = "nvim";
      BAT_THEME = "base16";
    };

    file = {
      # Neovim full config
      ".config/nvim" = {
        source = ../config/dot-config/nvim;
        recursive = true;
      };

      # Emacs config
      ".config/emacs" = {
        source = ../config/dot-config/emacs;
        recursive = true;
      };

      # Doom Emacs config
      ".config/doom" = {
        source = ../config/dot-config/doom;
        recursive = true;
      };

      # Zed editor
      ".config/zed" = {
        source = ../config/dot-config/zed;
        recursive = true;
      };

      # k9s
      ".config/k9s" = {
        source = ../config/dot-config/k9s;
        recursive = true;
      };

      # Aerospace (macOS tiling WM)
      ".config/aerospace" = {
        source = ../config/dot-config/aerospace;
        recursive = true;
      };

      # Karabiner configuration
      ".config/karabiner" = {
        source = ../karabiner;
        recursive = true;
      };

      # Borders (macOS window borders)
      ".config/borders/bordersrc".source = ../config/dot-config/borders/bordersrc;
    };
  };

  # Let Home Manager manage itself
  programs.home-manager.enable = true;

  # Enable font configuration
  fonts.fontconfig.enable = true;
}
