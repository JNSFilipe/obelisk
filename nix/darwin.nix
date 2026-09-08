{
  config,
  homeDirectory,
  hostName,
  pkgs,
  userName,
  ...
}:
{
  imports = [ ./homebrew.nix ];

  # ── Nix settings ───────────────────────────────────────────────────────────

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    auto-optimise-store = false; # true can be slow on macOS
    trusted-users = [
      "root"
      userName
    ];
  };

  # Allow unfree packages (needed for some nixpkgs entries)
  nixpkgs.config.allowUnfree = true;

  # Generated nix-darwin docs can break when nix-darwin and nixpkgs briefly
  # disagree on nixos-render-docs flags. They are not needed for activation.
  documentation.enable = false;
  documentation.doc.enable = false;
  system.tools.darwin-uninstaller.enable = false;

  # ── System identity ─────────────────────────────────────────────────────────

  networking.hostName = hostName;
  networking.computerName = hostName;
  system.primaryUser = userName;

  # ── Users ───────────────────────────────────────────────────────────────────

  users.users.${userName} = {
    home = homeDirectory;
    shell = pkgs.zsh;
  };

  # ── Shell ──────────────────────────────────────────────────────────────────

  # Make zsh the default system shell (home-manager configures the user shell)
  programs.zsh.enable = true;

  # nix-darwin copies GUI bundles from system packages into
  # /Applications/Nix Apps so Launch Services and Spotlight can index them.
  environment.systemPackages = [
    config.home-manager.users.${userName}.programs.doom-emacs.finalEmacsPackage
    pkgs.kitty
  ];

  # nix-darwin does not add Homebrew to PATH. Homebrew is the default package
  # source (nix/homebrew.nix), so its bin dirs must be reachable in every
  # shell. Casks are /Applications GUI apps and never need PATH. Appended (not
  # prepended) so the few nix bins — home-manager programs.* modules — still
  # win on collisions.
  #
  # Keg-only formulae are not linked into /opt/homebrew/bin, so their opt dirs
  # are listed explicitly. openblas is keg-only too but library-only: builds
  # take /opt/homebrew/opt/openblas/lib/pkgconfig via PKG_CONFIG_PATH instead.
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
    "/opt/homebrew/opt/llvm/bin"
    "/opt/homebrew/opt/rustup/bin"
    "/opt/homebrew/opt/postgresql@18/bin"
  ];

  # ── Tailscale ────────────────────────────────────────────────────────────────

  services.tailscale.enable = true;

  # ── macOS defaults ──────────────────────────────────────────────────────────

  system.defaults = {
    dock = {
      autohide = true;
      show-recents = true;
      minimize-to-application = false;
    };
    NSGlobalDomain = {
      AppleInterfaceStyle = "Dark";
      # Set "Icon & widget style" to dark (macOS Sequoia+)
      AppleIconAppearanceTheme = "RegularDark";
      AppleShowAllExtensions = true;
      # Fast key repeat
      InitialKeyRepeat = 15;
      KeyRepeat = 2;
      # Disable smart quotes / dashes in code contexts
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticDashSubstitutionEnabled = false;
    };
    trackpad.Clicking = true;
    finder = {
      AppleShowAllFiles = true;
      ShowPathbar = true;
      ShowStatusBar = true;
      _FXShowPosixPathInTitle = true;
    };
    screensaver.askForPasswordDelay = 0;
  };

  # ── State version ───────────────────────────────────────────────────────────
  # Do NOT change after initial install.
  system.stateVersion = 5;
}
