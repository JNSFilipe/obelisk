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

  # Pilish uses Homebrew's Pi CLI.  Runtime exceptions, each checked against
  # formulae and casks, and allowlisted by name in flake.nix:
  home.packages = [
    # Antigravity's ACP server, for Gloom's M-x Machina (agent-shell).  The
    # antigravity-cli cask ships only the `agy` CLI.  Unfree.
    pkgs.antigravity-acp
  ];
}
