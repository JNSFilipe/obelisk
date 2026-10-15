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

  outputs =
    inputs@{
      self,
      nixpkgs,
      nix-darwin,
      home-manager,
      ...
    }:
    let
      inherit (nixpkgs) lib;

      system = "aarch64-darwin";
      userName = "jfilipe";
      homeDirectory = "/Users/${userName}";
      flakeRoot = "${homeDirectory}/Documents/GitHub/obelisk";
      pkgs = nixpkgs.legacyPackages.${system};

      # ── Machines ────────────────────────────────────────────────────────────
      # Every Mac this repository manages, mapped to its profile.  The profile
      # picks the Homebrew package set (nix/homebrew.nix); anything else that
      # differs lives in nix/hosts/<name>.nix.  Adding a machine is one line
      # here plus that file.
      hosts = {
        gauss = "personal";
        thorp = "work";
      };

      statixConfig = pkgs.writeText "statix.toml" ''
        disabled = [
          # Nix modules are clearer when related option paths stay in domain sections.
          "repeated_keys"
        ]
      '';

      mkDarwinConfiguration =
        hostName: hostProfile: extraModules:
        nix-darwin.lib.darwinSystem {
          inherit system;
          modules = [
            ./nix/darwin.nix
            ./nix/hosts/${hostName}.nix
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
                    hostName
                    hostProfile
                    inputs
                    userName
                    ;
                };
              };
            }
          ]
          ++ extraModules;
          specialArgs = {
            inherit
              homeDirectory
              hostName
              hostProfile
              inputs
              userName
              ;
          };
        };

      # A pure configuration variant: sudo's environment filtering cannot
      # accidentally turn a locked activation into a Homebrew upgrade.
      lockedModule = {
        homebrew.onActivation = {
          autoUpdate = lib.mkForce false;
          upgrade = lib.mkForce false;
        };
      };

      configurations = lib.mapAttrs (hostName: profile: mkDarwinConfiguration hostName profile [ ]) hosts;

      lockedConfigurations = lib.mapAttrs' (
        hostName: profile:
        lib.nameValuePair "${hostName}-locked" (mkDarwinConfiguration hostName profile [ lockedModule ])
      ) hosts;
    in
    {
      darwinConfigurations = configurations // lockedConfigurations;

      formatter.${system} = pkgs.nixfmt-tree;

      checks.${system} =
        # Build every machine, so a change cannot quietly break the one that is
        # not in front of you.
        lib.mapAttrs (_: configuration: configuration.system) configurations // {
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
