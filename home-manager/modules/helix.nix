{ config, pkgs, ... }:

{
  programs.helix = {
    enable = true;

    settings = {
      theme = "everforest_dark";

      editor = {
        line-number = "relative";
        mouse = true;
        rulers = [ 120 ];
        cursorline = true;
        color-modes = true;
        bufferline = "multiple";
        auto-format = true;
        end-of-line-diagnostics = "hint";

        inline-diagnostics = {
          cursor-line = "warning";
        };

        lsp = {
          auto-signature-help = true;
          display-messages = true;
          display-inlay-hints = true;
        };

        indent-guides = {
          render = true;
          character = "|";
        };

        cursor-shape = {
          insert = "bar";
        };

        file-picker = {
          hidden = false;
        };

        statusline = {
          left = [ "mode" "spinner" ];
          center = [ "file-name" "version-control" ];
          right = [
            "diagnostics"
            "selections"
            "position-percentage"
            "position"
            "file-encoding"
            "file-line-ending"
            "file-type"
          ];
          separator = "│";
          mode = {
            normal = "NORMAL";
            insert = "INSERT";
            select = "SELECT";
          };
        };
      };
    };

    languages = {
      # Language-specific settings can be added here
      # Corresponds to languages.toml
    };
  };
}
