{
  description = "obelisk — macOS system config via nix-darwin + home-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, nix-darwin, home-manager, ... }: {
    darwinConfigurations."gauss" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin"; # Change to "x86_64-darwin" for Intel Macs
      modules = [
        ./nix/darwin.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "bak-before-nix";
          home-manager.users.jfilipe = import ./nix/home.nix;
          home-manager.extraSpecialArgs = {
            inherit inputs;
            # Absolute path to this flake, used for mkOutOfStoreSymlink.
            # Keep this in sync with wherever the repo lives on disk.
            flakeRoot = "/Users/jfilipe/Documents/GitHub/obelisk";
          };
        }
      ];
      specialArgs = { inherit inputs; };
    };
  };
}
