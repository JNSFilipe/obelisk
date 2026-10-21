{ pkgs, ... }:
{
  # ── nixpkgs escape hatch ────────────────────────────────────────────────────
  # Homebrew is the package source whenever available (nix/homebrew.nix).
  # Add a package here only after checking both formulae and casks with
  # `brew info --formula <name>` and `brew info --cask <name>`. Move it to
  # homebrew.nix when Homebrew gains support.
  #
  # Home Manager configuration is not an exception: available tools and
  # plugins belong in homebrew.nix even when Nix renders their config.
  # The remaining tmux plugins (sensible, yank, catppuccin) have no Homebrew
  # formulae and are referenced directly by programs.tmux.

  home.packages = [
    # ACP adapter that lets Emacs agent-shell drive the Claude Agent.
    # homebrew-core has no formula for it (its sibling codex-acp does, and
    # lives in homebrew.nix accordingly).
    pkgs.claude-agent-acp
  ];
}
