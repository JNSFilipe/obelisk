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
    ];

    # ── Casks (GUI applications) ─────────────────────────────────────────────

    casks = [
      # ── Terminals ──────────────────────────────────────────────────────────
      "ghostty"
      "kitty"
      "wezterm"

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

      # ── Browsers ───────────────────────────────────────────────────────────
      "firefox"

      # AI coding assistants moved to packages.nix (nix-managed)

      # ── Fonts ──────────────────────────────────────────────────────────────
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
      "marta"           # file manager
      "adguard"
      "raspberry-pi-imager"
      "hp-easy-start"
    ];
  };
}
