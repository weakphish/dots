{ config, pkgs, ... }:

{
  services.dunst = {
    enable = true;

    settings = {
      global = {
        font = "JetBrainsMono Nerd Font 10";
        frame_width = 2;
        corner_radius = 10;
      };

      urgency_low = {
        frame_color = "#7FBBB3";
        foreground = "#D3C6AA";
        background = "#2D353B";
        timeout = 5;
      };

      urgency_normal = {
        frame_color = "#A7C080";
        foreground = "#D3C6AA";
        background = "#2D353B";
        timeout = 10;
      };

      urgency_critical = {
        frame_color = "#E67E80";
        foreground = "#D3C6AA";
        background = "#2D353B";
        timeout = 10;
      };
    };
  };
}
