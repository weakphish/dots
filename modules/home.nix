# Recall that this is a function definition taking an attribute set as an argument.
# The attribute set has lib, pkgs, and any other arbitrary attributes that will be ignored.
# Also, recall that every Nix file is a single expression (a function, in this case).
{ lib, pkgs, ... }:
{
	home = {
		packages = with pkgs; [
			hello
			cowsay
			lolcat
			neofetch
				
			# Terminal utils
			neovim
			lsd
			bat
			zoxide
			starship
			lazygit
			lazydocker
		];

		username = "jack";
		homeDirectory = "/home/jack";

		stateVersion = "25.11"; # revert to 23.11 if broken

		# Testing out file stuff
		file = {
			"hello.txt".text = "Hello, world!";
		}
	};
}
