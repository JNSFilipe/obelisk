{ pkgs, ... }:
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
