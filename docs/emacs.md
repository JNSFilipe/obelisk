# Emacs installation and maintenance

Homebrew's `d12frosted/emacs-plus/emacs-plus-app` supplies Emacs and its matching
client. Nix declares that cask, shell wrappers, config links and the single
`org.nix-community.home.emacs` launchd agent. Do not enable a second daemon with
`brew services`.

Doom lives in the writable `~/.local/share/doom-emacs` checkout; its packages and
native compilation cache live under that checkout's `.local`. Home Manager links
`~/.config/emacs` there and `~/.config/doom` to this repository's `configs/doom`.
Doom packages are built with `/opt/homebrew/bin/emacs`, never the old Nix Emacs.
This is conventional Doom managed alongside Nix, not a Nix-built Doom bundle.

## Setup and migration

From this repository, as your normal user:

```sh
brew install --cask d12frosted/emacs-plus/emacs-plus-app
make doom-install
make switch-locked
```

The first two steps have already been performed on this machine during the
migration trial. `make switch-locked` needs your sudo password. It uses a separate
Nix configuration with Homebrew updates/upgrades disabled; missing declared
packages can still be installed. Save your buffers and finish terminal jobs
before switching because the Emacs service is replaced.

Home Manager preserves the former `~/.config/emacs` directory as
`~/.config/emacs.bak-before-nix`. If that backup already exists, resolve the
collision manually rather than deleting either copy. The legacy repository
`configs/emacs` is not changed or used.

After activation, run `ec` for a GUI frame and `et` for a terminal frame. Both
connect to launchd's daemon; neither starts an independent fallback daemon.
`EDITOR` and `VISUAL` wait for editing to finish, as callers such as Git expect.
Use **Emacs Client.app** from Finder or the Dock for client frames. The separate
Emacs.app opens a standalone instance when explicitly launched.

## Everyday changes

```sh
make doom-sync       # after init.el/packages.el changes or an Emacs upgrade
make doom-update     # upgrade Doom and its package pins
doom doctor          # diagnose optional tool dependencies
```

Both maintenance targets use the Homebrew executable and prepare Ghostel's
official native module before the GUI needs it. Downloads require network
access. Normal `config.el` edits only need a reload or restart.

After saving buffers and finishing terminal jobs, restart the supervised daemon:

```sh
launchctl kickstart -k "gui/$(id -u)/org.nix-community.home.emacs"
```

`make switch` updates Nix and declared Homebrew packages, but Doom upgrades are
explicit. If Homebrew upgrades Emacs, run `make doom-sync` before resuming work.
`vanilla-emacs` uses the same Homebrew executable with `configs/vemacs`.

## Rollback

`make rollback` restores Nix's previous system and Home Manager generation,
including the previous daemon configuration while its store paths remain.
It does not downgrade Homebrew or revert the writable Doom checkout/packages.
Keep the old Nix generations and `~/.config/emacs.bak-before-nix` until satisfied.
If restoring the old config directory is needed, first move the new
`~/.config/emacs` symlink aside, then restore the backup; preserve both installations.

## Validation and limits

The migration trial used Emacs Plus 31.1, conventional Doom 2.2.4, GUI and TTY
clients, actual native compilation, and Ghostel's official native module.
All 19 Sheprd regression tests and its live Ghostel/workspace integration checks
passed. The Doom GUI exposes its window and controls through macOS Accessibility.
Actual Tangrid tiling remains a manual acceptance check: its automation interface
did not expose usable tiling controls during this trial.

`doom doctor` also reported optional language-tool gaps (Go helpers, Terraform,
Python tools, shellcheck, nixfmt and web formatters), missing Symbola, and missing
JavaScript `+tree-sitter` flags. These are not resolved by changing the Emacs
package source. Its duplicate-config warnings describe the pre-activation trial.
