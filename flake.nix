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
          # Fail closed if a programs.* default silently reintroduces a Nix
          # application. These are configuration artifacts, custom wrappers,
          # or tools with no Homebrew distribution, not package exceptions.
          homebrew-runtime-ownership =
            let
              allowedHomeArtifacts = [
                "emacs"
                "doom"
                "vanilla-emacs"
                "gloom-emacs"
                "home-configuration-reference-manpage"
                "hm-session-vars.sh"
              ];
              allowedSystemPackages = [
                "darwin-version"
                "darwin-rebuild"
                "darwin-option"
                "nix"
                "nix-info"
              ];
              requiredFormulae = [
                "atuin"
                "bash"
                "carapace"
                "fzf"
                "fzf-tab"
                "git"
                "git-lfs"
                "helix"
                "lazygit"
                "man-db"
                "nushell"
                "opam"
                "pi-coding-agent"
                "pure"
                "texinfo"
                "tmux"
                "yazi"
                "zoxide"
                "zsh"
                "zsh-autocomplete"
                "zsh-autosuggestions"
              ];
              requiredCasks = [
                "claude-code@latest"
                "kitty"
                "omniwm"
                "purge"
                "tailscale-app"
                "zen"
              ];
              compliant =
                configuration:
                let
                  cfg = configuration.config;
                  hm = cfg.home-manager.users.${userName};
                  allowed = names: packages: lib.all (p: builtins.elem (lib.getName p) names) packages;
                in
                lib.all (
                  p: builtins.elem p.name allowedHomeArtifacts || lib.getName p == "home-manager"
                ) hm.home.packages
                && allowed allowedSystemPackages cfg.environment.systemPackages
                && toString cfg.users.users.${userName}.shell == "/opt/homebrew/bin/zsh"
                && hm.programs.zsh.plugins == [ ]
                && !lib.hasInfix "/nix/store/" hm.home.file."./.zshrc".text
                && lib.all (name: builtins.elem name (map (b: b.name) cfg.homebrew.brews)) requiredFormulae
                && lib.all (name: builtins.elem name (map (c: c.name) cfg.homebrew.casks)) requiredCasks
                && !(builtins.elem "claude-code" (map (c: c.name) cfg.homebrew.casks));
            in
            assert lib.assertMsg (lib.all compliant (builtins.attrValues configurations))
              "Homebrew-first package ownership regressed; audit new Nix runtime packages before adding them to the allowlist.";
            pkgs.runCommand "homebrew-runtime-ownership" { } ''
              touch "$out"
            '';

          pi-settings-merge = pkgs.runCommand "pi-settings-merge" { nativeBuildInputs = [ pkgs.python3 ]; } ''
            PI_SYNC_PACKAGES_SCRIPT=${./configs/scripts/pi-sync-packages.py} \
              python3 ${./tests/pi-packages.py}
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
