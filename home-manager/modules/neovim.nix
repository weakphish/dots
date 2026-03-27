{ config, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;

    # LSP and development dependencies
    extraPackages = with pkgs; [
      # LSP servers
      lua-language-server
      nil # Nix LSP
      nodePackages.typescript-language-server
      nodePackages.bash-language-server
      gopls
      rust-analyzer
      ruff
      basedpyright

      # Formatters
      stylua
      nixpkgs-fmt
      nodePackages.prettier
      gofumpt
      rustfmt

      # Tools
      gcc # For treesitter compilation
      gnumake
      tree-sitter
    ];
  };

  # Note: The full neovim config is symlinked from config/dot-config/nvim
  # in home.nix since it uses lazy.nvim and has a complex plugin setup
}
