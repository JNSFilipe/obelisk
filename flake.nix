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
    nix-doom-emacs-unstraightened = {
      url = "github:marienz/nix-doom-emacs-unstraightened";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nix-darwin,
      home-manager,
      ...
    }:
    let
      system = "aarch64-darwin";
      hostName = "gauss";
      userName = "jfilipe";
      homeDirectory = "/Users/${userName}";
      flakeRoot = "${homeDirectory}/Documents/GitHub/obelisk";
      pkgs = nixpkgs.legacyPackages.${system};
      statixConfig = pkgs.writeText "statix.toml" ''
        disabled = [
          # Nix modules are clearer when related option paths stay in domain sections.
          "repeated_keys"
        ]
      '';

      darwinConfiguration = nix-darwin.lib.darwinSystem {
        inherit system;
        modules = [
          ./nix/darwin.nix
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "bak-before-nix";
              users.${userName} = import ./nix/home.nix;
              extraSpecialArgs = {
                inherit
                  flakeRoot
                  homeDirectory
                  inputs
                  userName
                  ;
              };
            };
          }
        ];
        specialArgs = {
          inherit
            homeDirectory
            hostName
            inputs
            userName
            ;
        };
      };
    in
    {
      darwinConfigurations.${hostName} = darwinConfiguration;

      formatter.${system} = pkgs.nixfmt-tree;

      checks.${system} = {
        inherit (darwinConfiguration) system;
        kanata-config = pkgs.runCommand "kanata-config-check" { nativeBuildInputs = [ pkgs.kanata ]; } ''
          kanata --check --cfg ${./configs/kanata/kanata.kbd}
          touch "$out"
        '';
        nix-static-analysis =
          pkgs.runCommand "nix-static-analysis"
            {
              nativeBuildInputs = [
                pkgs.deadnix
                pkgs.statix
              ];
            }
            ''
                      deadnix --fail ${self}/flake.nix ${self}/nix
              statix check --config ${statixConfig} ${self}/flake.nix
              statix check --config ${statixConfig} ${self}/nix
                      touch "$out"
            '';
      };
    };
}
