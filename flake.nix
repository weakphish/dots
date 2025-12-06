{
  description = "My Home Manager configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.11";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

	elephant.url = "github:abenz1267/elephant";

	walker = {
	  url = "github:abenz1267/walker";
	  inputs.elephant.follows = "elephant";
	};
  };

  outputs = { nixpkgs, home-manager, ... }:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      homeConfigurations = {
        jack = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./modules/home.nix ];
        };
      };
    };
}
