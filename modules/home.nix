# Recall that this is a function definition taking an attribute set as an argument.
# The attribute set has lib, pkgs, and any other arbitrary attributes that will be ignored.
# Also, recall that every Nix file is a single expression (a function, in this case).
{ lib, pkgs, ... }: let
	username = "jack";
in {
	home = {
		inherit username;
		homeDirectory = "/home/${username}";
		stateVersion = "25.11"; # revert to 23.11 if broken

		packages = with pkgs; [
			# Me shell
			zsh
				
			# Terminal utils
			neovim
			neofetch
			lsd
			bat
			zoxide
			starship
			lazygit
			lazydocker
		];
		
		
	};
	programs = {
		# let home-manager manage itself
		home-manager.enable = true;

		# It's easier to just link my config to XDG and source it from nvim
		# See xdg.configFile attribute below
		neovim = {
			extraConfig = lib.fileContents ":luafile ~/.config/nvim/init.lua";
		};

		zsh = {
			enable = true;
			shellAliases = {
				lg = "lazygit";
				tf ="terraform";
				p = "poetry run";
				dc = "docker compose";
				k = "kubectl";
				ls = "lsd";
				la = "lsd -la";
			};
			initContent = ''
				# Set up Zoxide
				eval "$(zoxide init zsh)"

				# Set up Starship
				eval "$(starship init zsh)"
			'';
		};
	};

	xdg.configFile.nvim = {
		source = ../config/nvim;
		recursive = true;
	};
}
