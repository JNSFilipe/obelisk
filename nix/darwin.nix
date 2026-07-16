{
  config,
  homeDirectory,
  hostName,
  lib,
  pkgs,
  userName,
  ...
}:
let
  kanataRuntimeApp = "/Applications/Kanata.app";
  kanataRuntimeBin = "${kanataRuntimeApp}/Contents/MacOS/kanata";
  kanataRuntimeCfg = "/Library/Application Support/Kanata/kanata.kbd";
  kanataSourceCfg = ../configs/kanata/kanata.kbd;
  kanataConfigHash = builtins.hashFile "sha256" kanataSourceCfg;
  kanataRuntime = pkgs.runCommand "kanata-runtime" { nativeBuildInputs = [ pkgs.darwin.cctools ]; } ''
    mkdir -p "$out/Contents/MacOS" "$out/Contents/Frameworks"
    cp "${pkgs.kanata}/bin/kanata" "$out/Contents/MacOS/kanata"
    cp "${pkgs.libiconv}/lib/libiconv.2.dylib" "$out/Contents/Frameworks/libiconv.2.dylib"
    cp "${pkgs.libiconv}/lib/libcharset.1.dylib" "$out/Contents/Frameworks/libcharset.1.dylib"
    chmod 0755 "$out/Contents/MacOS/kanata" "$out/Contents/Frameworks/"*.dylib

    install_name_tool \
      -change "${pkgs.libiconv}/lib/libiconv.2.dylib" \
      '@executable_path/../Frameworks/libiconv.2.dylib' \
      "$out/Contents/MacOS/kanata"
    install_name_tool \
      -change "${pkgs.libiconv}/lib/libcharset.1.dylib" \
      '@loader_path/libcharset.1.dylib' \
      -id '@rpath/libiconv.2.dylib' \
      "$out/Contents/Frameworks/libiconv.2.dylib"
    install_name_tool \
      -id '@rpath/libcharset.1.dylib' \
      "$out/Contents/Frameworks/libcharset.1.dylib"

    "$out/Contents/MacOS/kanata" --check --cfg "${kanataSourceCfg}"
  '';
  kanataPackageMarker = pkgs.writeText "kanata-package" ''
    package=${pkgs.kanata}
    signature=identifier-v1
    runtime=${kanataRuntime}
  '';
  kanataInfoPlist = pkgs.writeText "kanata-Info.plist" ''
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
  '';
  karabinerVhidDaemon = "/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-VirtualHIDDevice-Daemon.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Daemon";
in
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

  system.activationScripts.preActivation.text = lib.mkBefore ''
    # Put the actual executable in the stable app bundle. A shell wrapper that
    # execs a Nix-store binary makes TCC authorize the versioned store path.
    /usr/bin/install -d -m 0755 \
      "${kanataRuntimeApp}/Contents/Frameworks" \
      "${kanataRuntimeApp}/Contents/MacOS" \
      "${kanataRuntimeApp}/Contents/Resources"
    app_changed=0
    # codesign modifies the Mach-O binary, so compare a signed package marker
    # rather than comparing the installed executable with the store binary.
    if ! /usr/bin/cmp -s "${kanataPackageMarker}" "${kanataRuntimeApp}/Contents/Resources/nix-package"; then
      /usr/bin/install -m 0755 "${kanataRuntime}/Contents/MacOS/kanata" "${kanataRuntimeBin}"
      /usr/bin/install -m 0755 \
        "${kanataRuntime}/Contents/Frameworks/libcharset.1.dylib" \
        "${kanataRuntimeApp}/Contents/Frameworks/libcharset.1.dylib"
      /usr/bin/install -m 0755 \
        "${kanataRuntime}/Contents/Frameworks/libiconv.2.dylib" \
        "${kanataRuntimeApp}/Contents/Frameworks/libiconv.2.dylib"
      /usr/bin/install -m 0644 "${kanataPackageMarker}" "${kanataRuntimeApp}/Contents/Resources/nix-package"
      /usr/bin/codesign --force --sign - "${kanataRuntimeApp}/Contents/Frameworks/libcharset.1.dylib"
      /usr/bin/codesign --force --sign - "${kanataRuntimeApp}/Contents/Frameworks/libiconv.2.dylib"
      app_changed=1
    fi
    if ! /usr/bin/cmp -s "${kanataInfoPlist}" "${kanataRuntimeApp}/Contents/Info.plist"; then
      /usr/bin/install -m 0644 "${kanataInfoPlist}" "${kanataRuntimeApp}/Contents/Info.plist"
      app_changed=1
    fi
    if [ "$app_changed" -eq 1 ] || ! /usr/bin/codesign --verify --strict "${kanataRuntimeApp}" >/dev/null 2>&1; then
      /usr/bin/codesign --force --sign - \
        --identifier org.nix-community.kanata \
        --requirements '=designated => identifier "org.nix-community.kanata"' \
        "${kanataRuntimeApp}"
    fi

    /usr/bin/install -d -m 0755 "/Library/Application Support/Kanata"
    if ! /usr/bin/cmp -s "${kanataSourceCfg}" "${kanataRuntimeCfg}"; then
      /usr/bin/install -m 0644 "${kanataSourceCfg}" "${kanataRuntimeCfg}"
    fi
  '';

  system.activationScripts.postActivation.text = lib.mkAfter ''
    wait_for_launchd_service() {
      service="$1"
      attempts=0
      stable_checks=0

      if ! service_details="$(launchctl print "$service" 2>/dev/null)" || [[ "$service_details" != *"state = running"* ]]; then
        launchctl kickstart -k "$service"
      fi

      while [ "$attempts" -lt 60 ]; do
        if service_details="$(launchctl print "$service" 2>/dev/null)" && [[ "$service_details" == *"state = running"* ]]; then
          stable_checks=$((stable_checks + 1))
          if [ "$stable_checks" -ge 16 ]; then
            return 0
          fi
        else
          stable_checks=0
        fi
        /bin/sleep 0.25
        attempts=$((attempts + 1))
      done

      echo "launchd service failed to reach running state: $service" >&2
      launchctl print "$service" >&2 || true
      return 1
    }

    wait_for_launchd_service system/org.pqrs.Karabiner-VirtualHIDDevice-Daemon
    wait_for_launchd_service system/org.nix-community.kanata
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
      KeepAlive.SuccessfulExit = false;
      ThrottleInterval = 5;
      EnvironmentVariables = {
        OBELISK_KANATA_PACKAGE = "${pkgs.kanata}";
        OBELISK_KANATA_PACKAGE_MARKER = "${kanataPackageMarker}";
        OBELISK_KANATA_CONFIG_SHA256 = kanataConfigHash;
        OBELISK_KANATA_APP_METADATA = "${kanataInfoPlist}";
      };
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

  # nix-darwin does not add Homebrew to PATH. Append it so brew-only formulae
  # (herdr, biber, …) are reachable in the shell. Casks are /Applications GUI
  # apps and never need PATH. Appended (not prepended) so nix bins win on
  # collisions (e.g. doxx/ty/rtk also live in packages.nix).
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
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
