#!/usr/bin/env just --justfile

default:
	@just --list

rebuild: clean update

update:
	home-manager switch --flake .#jack

clean:
	nix-collect-garbage -d
