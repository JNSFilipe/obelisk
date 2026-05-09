{ ... }:
{
  # ── nix-darwin homebrew integration ────────────────────────────────────────
  # GUI apps only.  All CLI tools live in nix/packages.nix.
  # Exceptions: brews[] holds CLI tools not yet packaged in nixpkgs;
  #             some casks (claude-code, copilot-cli, …) are CLIs
  #             distributed only as macOS casks — no nix alternative.

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate  = true;
      upgrade     = true;
      # "zap" removes anything not listed below; "cleanup" removes only
      # unlisted formulae/casks. Use "cleanup" while transitioning.
      cleanup     = "uninstall";
    };

    # ── Taps ─────────────────────────────────────────────────────────────────

    taps = [
      "d12frosted/emacs-plus" # needed for emacs-plus-app cask
    ];

    # ── CLI exceptions (not in nixpkgs — move to packages.nix when available)
    brews = [
      "doxx"       # view .docx in terminal
      "codex-acp"  # GitHub Copilot ACP
      "ty"         # Astral ty type checker
      "rtk"        # For saving tokes: Yields chewedup output for common terminal tools
    ];

    # ── Casks (GUI applications) ─────────────────────────────────────────────

    casks = [
      # ── Terminals ──────────────────────────────────────────────────────────
      # "ghostty"
      # "kitty"
      "wezterm"
      "warp"

      # ── Editors ────────────────────────────────────────────────────────────
      "emacs-plus-app"   # from d12frosted/emacs-plus tap
      "visual-studio-code"
      "sublime-text"
      "datagrip"
      "zed"

      # ── Version control ────────────────────────────────────────────────────
      "github"           # GitHub Desktop

      # ── Window / keyboard management ──────────────────────────────────────
      "karabiner-elements"
      "leader-key"
      "dockdoor"
      "wooshy" # For navigation whith keyboard... still experimenting with this

      # ── Browsers ───────────────────────────────────────────────────────────
      "firefox"
      "helium-browser"

      # AI coding assistants moved to packages.nix (nix-managed)

      # ── Fonts ──────────────────────────────────────────────────────────────
      # TODO: The fonts should be managed on the nix side of things
      "font-iosevka"
      "font-jetbrains-mono-nerd-font"
      "font-fira-code-nerd-font"
      "font-fira-mono-nerd-font"
      "font-source-code-pro"
      "font-geist-mono"
      "font-symbols-only-nerd-font"

      # ── LaTeX ──────────────────────────────────────────────────────────────
      "mactex"

      # ── Productivity ───────────────────────────────────────────────────────
      "raycast"
      "obsidian"
      "notion-calendar"
      "clickup"
      "bitwarden"

      # ── Communication ──────────────────────────────────────────────────────
      "whatsapp"
      "zoom"
      "microsoft-teams"
      "microsoft-outlook"

      # ── Microsoft Office ───────────────────────────────────────────────────
      "microsoft-word"
      "microsoft-excel"
      "microsoft-powerpoint"

      # ── Storage / cloud ────────────────────────────────────────────────────
      "google-drive"
      "daisydisk"
      "the-unarchiver"
      "paragon-extfs"

      # ── Development tools ──────────────────────────────────────────────────
      "docker-desktop"
      "postman"

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
      "raspberry-pi-imager"
      "hp-easy-start"
    ];
  };
}
