{ config, pkgs, lib, ... }:
let
  kanataRuntimeApp = "/Applications/Kanata.app";
  kanataRuntimeBin = "${kanataRuntimeApp}/Contents/MacOS/kanata";
  kanataRuntimeCfg = "/Library/Application Support/Kanata/kanata.kbd";
  karabinerVhidDaemon = "/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-VirtualHIDDevice-Daemon.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Daemon";
in
{
  imports = [ ./homebrew.nix ];

  # ── Nix settings ───────────────────────────────────────────────────────────

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    auto-optimise-store   = false; # true can be slow on macOS
    trusted-users         = [ "@admin" "jfilipe" ];
  };

  # Allow unfree packages (needed for some nixpkgs entries)
  nixpkgs.config.allowUnfree = true;

  # Generated nix-darwin docs can break when nix-darwin and nixpkgs briefly
  # disagree on nixos-render-docs flags. They are not needed for activation.
  documentation.enable = false;
  documentation.doc.enable = false;
  system.tools.darwin-uninstaller.enable = false;

  system.activationScripts.preActivation.text = lib.mkBefore ''
    launchctl bootout system/org.nix-community.kanata 2>/dev/null || true
    pkill -f "/Applications/Kanata.app/Contents/MacOS/kanata" 2>/dev/null || true

    # Put the actual executable in the stable app bundle. A shell wrapper that
    # execs a Nix-store binary makes TCC authorize the versioned store path.
    install -d -m 0755 "${kanataRuntimeApp}/Contents/MacOS"
    install -m 0755 "${pkgs.kanata}/bin/kanata" "${kanataRuntimeBin}"
    cat > "${kanataRuntimeApp}/Contents/Info.plist" <<'PLIST'
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>CFBundleExecutable</key>
      <string>kanata</string>
      <key>CFBundleIdentifier</key>
      <string>org.nix-community.kanata</string>
      <key>CFBundleName</key>
      <string>Kanata</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>LSBackgroundOnly</key>
      <true/>
    </dict>
    </plist>
    PLIST
    /usr/bin/codesign --force --sign - --identifier org.nix-community.kanata "${kanataRuntimeApp}"

    install -d -m 0755 "/Library/Application Support/Kanata"
    install -m 0644 "${../configs/kanata/kanata.kbd}" "${kanataRuntimeCfg}"
  '';

  system.activationScripts.postActivation.text = lib.mkAfter ''
    if [ -f /Library/LaunchDaemons/org.pqrs.Karabiner-VirtualHIDDevice-Daemon.plist ]; then
      launchctl bootstrap system /Library/LaunchDaemons/org.pqrs.Karabiner-VirtualHIDDevice-Daemon.plist 2>/dev/null || true
      launchctl kickstart -k system/org.pqrs.Karabiner-VirtualHIDDevice-Daemon 2>/dev/null || true
    fi

    if [ -f /Library/LaunchDaemons/org.nix-community.kanata.plist ]; then
      launchctl bootstrap system /Library/LaunchDaemons/org.nix-community.kanata.plist 2>/dev/null || true
      launchctl kickstart -k system/org.nix-community.kanata 2>/dev/null || true
    fi
  '';

  launchd.daemons.kanata = {
    serviceConfig = {
      Label = "org.nix-community.kanata";
      ProgramArguments = [
        kanataRuntimeBin
        "--cfg"
        kanataRuntimeCfg
        "--no-wait"
      ];
      RunAtLoad = true;
      KeepAlive = false;
      StandardOutPath = "/Library/Logs/kanata.log";
      StandardErrorPath = "/Library/Logs/kanata.error.log";
      ProcessType = "Interactive";
    };
  };

  launchd.daemons.karabiner-vhid-daemon = {
    serviceConfig = {
      Label = "org.pqrs.Karabiner-VirtualHIDDevice-Daemon";
      ProgramArguments = [
        karabinerVhidDaemon
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "/Library/Logs/karabiner-vhid-daemon.log";
      StandardErrorPath = "/Library/Logs/karabiner-vhid-daemon.error.log";
    };
  };

  # ── System identity ─────────────────────────────────────────────────────────

  networking.hostName     = "gauss";
  networking.computerName = "gauss";
  system.primaryUser      = "jfilipe";

  # ── Users ───────────────────────────────────────────────────────────────────

  users.users.jfilipe = {
    home  = "/Users/jfilipe";
    shell = pkgs.zsh;
  };

  # ── Shell ──────────────────────────────────────────────────────────────────

  # Make zsh the default system shell (home-manager configures the user shell)
  programs.zsh.enable = true;

  # nix-darwin copies GUI bundles from system packages into
  # /Applications/Nix Apps so Launch Services and Spotlight can index them.
  environment.systemPackages = [
    config.home-manager.users.jfilipe.programs.doom-emacs.finalEmacsPackage
  ];

  # nix-darwin does not add Homebrew to PATH. Append it so brew-only formulae
  # (herdr, biber, …) are reachable in the shell. Casks are /Applications GUI
  # apps and never need PATH. Appended (not prepended) so nix bins win on
  # collisions (e.g. doxx/ty/rtk also live in packages.nix).
  environment.systemPath = [ "/opt/homebrew/bin" "/opt/homebrew/sbin" ];

  # ── Tailscale ────────────────────────────────────────────────────────────────

  services.tailscale.enable = true;

  # ── macOS defaults ──────────────────────────────────────────────────────────

  system.defaults = {
    dock = {
      autohide                  = true;
      show-recents              = true;
      minimize-to-application   = false;
    };
    NSGlobalDomain = {
      AppleInterfaceStyle       = "Dark";
      # Set "Icon & widget style" to dark (macOS Sequoia+)
      AppleIconAppearanceTheme  = "RegularDark";
      AppleShowAllExtensions    = true;
      # Fast key repeat
      InitialKeyRepeat          = 15;
      KeyRepeat                 = 2;
      # Disable smart quotes / dashes in code contexts
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticDashSubstitutionEnabled  = false;
    };
    trackpad.Clicking = true;
    finder = {
      AppleShowAllFiles         = true;
      ShowPathbar               = true;
      ShowStatusBar             = true;
      _FXShowPosixPathInTitle   = true;
    };
    screensaver.askForPasswordDelay = 0;
  };

  # ── State version ───────────────────────────────────────────────────────────
  # Do NOT change after initial install.
  system.stateVersion = 5;
}
