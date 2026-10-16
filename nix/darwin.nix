{
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

  # The Homebrew tailscale-app cask owns the VPN and bundles its matching CLI.
  # Avoid a second tailscaled competing with the app's network extension.
  services.tailscale.enable = false;

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
      # Keyboard and scrolling feel: these are the values that were actually in
      # use on gauss, captured from the live defaults rather than chosen, so
      # thorp gets the same experience instead of a tuned-up one.

      # Hold-to-repeat, which is what governs how fast Delete chews through
      # text.  Units are 15 ms: 30 ms between repeats after a 225 ms delay.
      # These are the System Settings slider floors, not below them.
      InitialKeyRepeat = 15;
      KeyRepeat = 2;
      # Held keys repeat rather than opening the accent menu.
      ApplePressAndHoldEnabled = false;

      # Scrolling: "natural" direction off, and no swipe-to-navigate-pages.
      "com.apple.swipescrolldirection" = false;
      AppleEnableSwipeNavigateWithScrolls = false;
      # Trackpad pointer tracking speed (not scroll): the System Settings slider.
      "com.apple.trackpad.scaling" = 3.0;
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

  # Scroll-wheel and mouse tracking speed have no typed nix-darwin option, so
  # they go through the generic writer.  Same source as the block above: the
  # values gauss is actually running.
  system.defaults.CustomUserPreferences."NSGlobalDomain" = {
    "com.apple.scrollwheel.scaling" = 0.75;
    "com.apple.mouse.scaling" = 1.5;
  };

  # ── Keyboard ───────────────────────────────────────────────────────────────
  # Applied with `hidutil` by the activation script, which the
  # org.nixos.activate-system daemon re-runs at boot, so it survives restarts.
  # Note that hidutil replaces the whole mapping table: a mapping that is not
  # listed here is not applied at all.

  system.keyboard = {
    enableKeyMapping = true;

    # Fn <-> Left Control, both directions (0xFF00000003 <-> 0x7000000E0).
    swapLeftCtrlAndFn = true;

    userKeyMapping = [
      # Caps Lock -> F18, the usual hyper-key arrangement.  This was already
      # active on gauss, set outside Nix; it is declared here so the swap above
      # does not wipe it, and so it survives a reboot on both machines.
      {
        HIDKeyboardModifierMappingSrc = 30064771129; # 0x700000039 Caps Lock
        HIDKeyboardModifierMappingDst = 30064771181; # 0x70000006D F18
      }
    ];
  };

  # `system.stateVersion` is per-machine and lives in nix/hosts/<name>.nix.
}
