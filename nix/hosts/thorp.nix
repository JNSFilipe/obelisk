_: {
  # thorp — the work Mac.
  #
  # Shared configuration lives in nix/darwin.nix, nix/home.nix and
  # nix/homebrew.nix.  This file is only for what is true of this machine and
  # not the other one: macOS defaults, launchd agents, paths tied to hardware
  # attached here, and anything a work policy requires.
  #
  # The personal/work split of Homebrew packages is driven by the profile in
  # flake.nix, not from here, so a package belongs in nix/homebrew.nix.
  #
  # Nothing machine-specific yet.  Likely candidates when this machine is set
  # up for real: a VPN or MDM-mandated agent, a work certificate bundle, and
  # mounts or printers that only exist on the work network.

  # Per-machine by definition: set once at install time and never changed.
  # Keep whatever nix-darwin's current default is on the day thorp is first
  # activated; do not copy gauss's value if they have drifted apart.
  system.stateVersion = 5;
}
