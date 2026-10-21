{
  config,
  homeDirectory,
  hostName,
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
    shell = "/opt/homebrew/bin/zsh";
  };

  # ── Shell ──────────────────────────────────────────────────────────────────

  # nix-darwin's Zsh module unconditionally installs pkgs.zsh. Keep global
  # environment initialization, but let Homebrew supply the login shell.
  programs.zsh.enable = false;
  programs.bash.enable = false; # Its module also unconditionally installs Nix Bash.
  programs.info.enable = false; # Homebrew texinfo supplies the info viewer.
  environment.shells = [
    "/opt/homebrew/bin/zsh"
    "/opt/homebrew/bin/bash"
  ];
  environment.etc = {
    "bashrc" = {
      text = ''
        [ -r "/etc/bashrc_$TERM_PROGRAM" ] && . "/etc/bashrc_$TERM_PROGRAM"
        if [ -n "$__ETC_BASHRC_SOURCED" ] || [ -n "$NOSYSBASHRC" ]; then return; fi
        __ETC_BASHRC_SOURCED=1
        if [ -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]; then
          . ${config.system.build.setEnvironment}
        fi
        [[ $- != *i* ]] && return
        shopt -s checkwinsize
        . ${config.system.build.setAliases}
        ${config.environment.interactiveShellInit}
        ${config.programs.bash.interactiveShellInit}
        if [ -f /etc/bash.local ]; then . /etc/bash.local; fi
      '';
      knownSha256Hashes = [
        "444c716ac2ccd9e1e3347858cb08a00d2ea38e8c12fdc5798380dc261e32e9ef"
        "617b39e36fa69270ddbee19ddc072497dbe7ead840cbd442d9f7c22924f116f4"
        "6be16cf7c24a3c6f7ae535c913347a3be39508b3426f5ecd413e636e21031e66"
        "08ffbf991a9e25839d38b80a0d3bce3b5a6c84b9be53a4b68949df4e7e487bb7"
      ];
    };
    "zshenv" = {
      text = ''
        if [[ -n "''${__ETC_ZSHENV_SOURCED-}" ]]; then return; fi
        __ETC_ZSHENV_SOURCED=1
        if [[ -o rcs ]]; then
          if [[ -z "''${__NIX_DARWIN_SET_ENVIRONMENT_DONE-}" ]]; then
            . ${config.system.build.setEnvironment}
          fi
          ${config.environment.shellInit}
        fi
        if [[ -f /etc/zshenv.local ]]; then source /etc/zshenv.local; fi
      '';
      # Recognize the upstream installer files on a fresh machine too.
      knownSha256Hashes = [
        "d07015be6875f134976fce84c6c7a77b512079c1c5f9594dfa65c70b7968b65f"
        "4e8f7cb9b699511f4ba5f9d5f8de1c9f5efb5c607de88faf5f58b8b9cb38edbf"
      ];
    };
    "zprofile" = {
      text = ''
        if [[ -n "''${__ETC_ZPROFILE_SOURCED-}" ]]; then return; fi
        __ETC_ZPROFILE_SOURCED=1
        . ${config.system.build.setAliases}
        ${config.environment.loginShellInit}
        if [[ -f /etc/zprofile.local ]]; then source /etc/zprofile.local; fi
      '';
      knownSha256Hashes = [
        "db8422f92d8cff684e418f2dcffbb98c10fe544b5e8cd588b2009c7fa89559c5"
        "0235d3c1b6cf21e7043fbc98e239ee4bc648048aafaf6be1a94a576300584ef2"
        "f320016e2cf13573731fbee34f9fe97ba867dd2a31f24893d3120154e9306e92"
      ];
    };
    "zshrc" = {
      text = ''
        if [[ -n "''${__ETC_ZSHRC_SOURCED-}" || -n "''${NOSYSZSHRC-}" ]]; then return; fi
        __ETC_ZSHRC_SOURCED=1
        bindkey -e
        ${config.environment.interactiveShellInit}
        if [[ -f /etc/zshrc.local ]]; then source /etc/zshrc.local; fi
      '';
      knownSha256Hashes = [
        "19a2d673ffd47b8bed71c5218ff6617dfc5e8533b240b9ba79142a45f8823c23"
        "fb5827cb4712b7e7932d438067ec4852c8955a9ff0f55e282473684623ebdfa1"
        "4d1ab5704f9d167a042fecac0d056c8a79a8ebd71e032d3489536c8db9ffe3e0"
        "c5a00c072c920f46216454978c44df044b2ec6d03409dc492c7bdcd92c94a110"
        "40b0d8751adae5b0100a4f863be5b75613a49f62706427e92604f7e04d2e2261"
        "bf76c5ed8e65e616f4329eccf662ee91be33b8bfd33713ce9946f2fe94fea7fa"
        "2af1b563e389d11b76a651b446e858116d7a20370d9120a7e9f78991f3e5f336"
        "27274e44b88a1174787f9a3d437d3387edc4f9aaaf40356054130797f5dc7912"
      ];
    };
  };

  # nix-darwin does not add Homebrew to PATH. Homebrew is the default package
  # source (nix/homebrew.nix), so its bin dirs must be reachable in every
  # shell. Casks are /Applications GUI apps and never need PATH. Nix profiles
  # retain configuration wrappers and Nix-only tools, not duplicate apps.
  #
  # Keg-only formulae are not linked into /opt/homebrew/bin, so their opt dirs
  # are listed explicitly. openblas is keg-only too but library-only: builds
  # take /opt/homebrew/opt/openblas/lib/pkgconfig via PKG_CONFIG_PATH instead.
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
    "/opt/homebrew/opt/man-db/libexec/bin"
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
