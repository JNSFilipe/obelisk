{ ... }:
{
  # ── nix-darwin homebrew integration ────────────────────────────────────────
  # GUI apps only.  All CLI tools live in nix/packages.nix.
  # Exceptions: brews[] holds CLI tools not yet packaged in nixpkgs;
  #             some casks, such as claude, are CLIs
  #             distributed only as macOS casks — no nix alternative.

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate  = true;
      upgrade     = true;
      # "zap" removes anything not listed below; "cleanup" removes only
      # unlisted formulae/casks. Use "cleanup" while transitioning.
      cleanup     = "uninstall";
      extraFlags  = [ "--force-cleanup" ];
    };

    # ── Taps ─────────────────────────────────────────────────────────────────

    taps = [
      "d12frosted/emacs-plus" # needed for emacs-plus-app cask
    ];

    # ── CLI exceptions (not in nixpkgs — move to packages.nix when available)
    brews = [
      "herdr"
      "awscli"
    ];

    # ── Casks (GUI applications) ─────────────────────────────────────────────

    casks = [
      # ── Terminals ──────────────────────────────────────────────────────────
      # "kitty"
      # "wezterm"
      "cmux"
      "ghostty"

      # ── Editors ────────────────────────────────────────────────────────────
      "zed"
      "datagrip"
      # "antigravity"      # for google-ai stuff
      "sublime-text"
      "emacs-plus-app"   # from d12frosted/emacs-plus tap
      "antigravity-cli"
      "visual-studio-code"

      # ── Version control ────────────────────────────────────────────────────
      "github"           # GitHub Desktop

      # ── Window / keyboard management ──────────────────────────────────────
      # "kindavim"
      # "dockdoor"
      "hammerspoon"

      # ── Browsers ───────────────────────────────────────────────────────────
      "firefox"
      "helium-browser"

      # AI coding assistants moved to packages.nix (nix-managed)

      # ── Fonts ──────────────────────────────────────────────────────────────
      # TODO: The fonts should be managed on the nix side of things
      "font-iosevka"
      "font-geist-mono"
      "font-source-code-pro"
      "font-fira-code-nerd-font"
      "font-fira-mono-nerd-font"
      "font-symbols-only-nerd-font"
      "font-jetbrains-mono-nerd-font"

      # ── LaTeX ──────────────────────────────────────────────────────────────
      # "mactex" # The all emcompasing, space eater approach
      # "basictex" # The lightweight alternative

      # ── Productivity ───────────────────────────────────────────────────────
      "notion"
      "claude"
      "bitwarden"
      "codex-app"
      "notion-calendar"

      # ── Communication ──────────────────────────────────────────────────────
      "zoom"
      "whatsapp"
      "microsoft-teams"

      # ── Microsoft Office ───────────────────────────────────────────────────
      "microsoft-word"
      "microsoft-excel"
      "microsoft-powerpoint"

      # ── Storage / cloud ────────────────────────────────────────────────────
      "daisydisk"
      "google-drive"
      "paragon-extfs"
      "the-unarchiver"

      # ── Development tools ──────────────────────────────────────────────────
      "postman"
      # "docker-desktop"

      # ── Media / creative ───────────────────────────────────────────────────
      "gimp"
      "iina"
      "spotify"

      # ── Gaming ─────────────────────────────────────────────────────────────
      "steam"
      "nvidia-geforce-now"

      # ── Utilities ──────────────────────────────────────────────────────────
      # Look into Tokie file manager in the future
      "adguard"
      "rustdesk" # The free TeamViewer/AnyDesk solution
      "betterdisplay" # For creating virtual displays and stuf
      "hp-easy-start"
      "raspberry-pi-imager"
    ];
  };
}
