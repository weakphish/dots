#!/usr/bin/env just --justfile

default:
	@just --list

update:
	home-manager switch --flake .#jack

clean:
	nix-collect-garbage -d
