{ pkgs, ... }:
{
  # ── nixpkgs escape hatch ────────────────────────────────────────────────────
  # Homebrew is the default package source (nix/homebrew.nix). Add a package
  # here only when homebrew-core does not ship it — check with
  # `brew info --formula <name>` first — and move it to homebrew.nix once brew
  # gains a formula.
  #
  # It does not cover packages pulled in by home-manager programs.* modules
  # (git, tmux, zsh plugins, fzf, atuin, zoxide, kitty, ghostty, helix,
  # lazygit, yazi, doom-emacs) — those stay nix-managed because nix also
  # renders their configuration.

  home.packages = [
    # ACP adapter that lets Emacs agent-shell drive the Claude Agent.
    # homebrew-core has no formula for it (its sibling codex-acp does, and
    # lives in homebrew.nix accordingly).
    pkgs.claude-agent-acp
  ];
}
