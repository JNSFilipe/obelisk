_: {
  # ── nixpkgs escape hatch ────────────────────────────────────────────────────
  # Homebrew is the default package source (nix/homebrew.nix). Add a package
  # here only when homebrew-core does not ship it — check with
  # `brew info --formula <name>` first — and move it to homebrew.nix once brew
  # gains a formula.
  #
  # This list is deliberately empty: everything that used to live here exists
  # in homebrew-core. It does not cover packages pulled in by home-manager
  # programs.* modules (git, tmux, zsh plugins, fzf, atuin, zoxide, kitty,
  # ghostty, helix, lazygit, yazi, doom-emacs) — those stay nix-managed
  # because nix also renders their configuration.

  home.packages = [ ];
}
