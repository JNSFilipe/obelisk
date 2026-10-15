_: {
  # gauss — the personal Mac.
  #
  # Shared configuration lives in nix/darwin.nix, nix/home.nix and
  # nix/homebrew.nix.  This file is only for what is true of this machine and
  # not the other one: macOS defaults, launchd agents, paths tied to hardware
  # attached here, and anything a work policy would forbid.
  #
  # The personal/work split of Homebrew packages is driven by the profile in
  # flake.nix, not from here, so a package belongs in nix/homebrew.nix.

  # Per-machine by definition: set once at install time and never changed,
  # which is why it cannot live in the shared module.
  system.stateVersion = 5;
}
