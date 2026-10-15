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

On a machine that has never been activated, run `make switch` (or
`make switch-locked`) **first** instead: activation installs the Emacs Plus
cask that `make doom-install` requires, so the explicit `brew install` above is
only needed when bootstrapping Doom ahead of activation. Note that activation
also starts the Emacs launchd agent, which runs with
`--init-directory=~/.local/share/doom-emacs` and therefore creates that
directory before Doom is cloned; `doom-manage.sh` installs into it rather than
refusing, keeping whatever state the daemon already wrote.

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

The parallel [Gloom Emacs config](../configs/gloom/README.md) lives in `configs/gloom`.
It uses plain Elisp, reuses installed package libraries, and shares Sheprd and
Oxocarbon with Doom. Run its explicit setup once, then launch it directly with
`/opt/homebrew/bin/emacs --init-directory="$PWD/configs/gloom"`. Home Manager also
declares the `gloom-emacs` launcher for subsequent activations. Its state and
optional named daemon are separate from Doom.
The [Gloom parity audit](../configs/gloom/PARITY.md) records the retained packages,
Meow/UI differences, remaining Doom-specific gaps and external-service test limits.

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
Tiling was never exercised: Tangrid, the window manager in use during the trial,
exposed no usable tiling controls through its sparse Accessibility interface.
Tangrid has since been dropped in favour of OmniWM, so this check no longer
applies.

`doom doctor` also reported optional language-tool gaps (Go helpers, Terraform,
Python tools, shellcheck, nixfmt and web formatters), missing Symbola, and missing
JavaScript `+tree-sitter` flags. These are not resolved by changing the Emacs
package source. Its duplicate-config warnings describe the pre-activation trial.
