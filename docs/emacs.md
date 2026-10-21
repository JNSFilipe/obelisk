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

## Agentic workflows: Pilish and Sheprd

Doom uses [Pilish](https://github.com/dnouri/pilish) as its native Pi frontend,
replacing `agent-shell`. Straight installs it from `dnouri/pilish` together with
its Elisp dependencies. It uses `/opt/homebrew/bin/pi` over RPC, not ACP, so the
`claude-agent-acp` and `codex-acp` adapter declarations have been removed.
Claude Code and Codex themselves remain installed for terminal workflows.

| Doom leader key | Action |
|---|---|
| `SPC a p` | Start/focus this project's Pi session (`M-x pilish`) |
| `SPC a t` | Toggle Pi session windows |
| `SPC a s` | Browse Pi sessions |

Inside Pilish, `C-c C-c` sends a prompt, `C-c C-k` aborts, and `C-c C-p` opens
the command menu. Models, credentials, extensions, skills and sessions come from
the same Pi configuration as the terminal. Authenticate with `/login` in terminal
Pi if needed; no credentials are stored in the Emacs config. Extension tools and
basic UI dialogs work over RPC, but custom terminal widgets are not equivalent
to their terminal UI. Pilish offers Markdown tree-sitter grammar installation on
first use; accept the one-time setup if the grammars are missing.

`pilish-project-trust-policy` is `default`: Pilish respects Pi's saved trust
choices instead of upstream's automatic `--approve`. Trust a new project's `.pi`
resources in terminal Pi first if you want them available from Emacs.
Sheprd/Ghostel and its Claude/Codex terminal launchers remain unchanged. Gloom
continues to use that terminal workflow; this native frontend is configured in
Doom only.

After pulling this change, run `make doom-sync` and restart Emacs after saving
buffers/finishing terminal jobs. Before the next Nix activation on a machine
that has the former adapter installed, run:

```sh
brew uninstall --formula codex-acp
```

Otherwise the intentional Homebrew `cleanup = "check"` policy reports it as
undeclared and aborts. The former Nix-managed Claude adapter leaves the profile
on the next activation; old store generations/cached Doom packages may retain
inactive files until normal garbage collection. No application data is removed.

Configuration and offline, isolated RPC smoke tests (no model requests):

```sh
/opt/homebrew/bin/emacs -Q --batch --eval '(let ((root (expand-file-name (format "~/.local/share/doom-emacs/.local/straight/build-%s/" emacs-version)))) (dolist (directory (directory-files root t "^[^.]")) (when (file-directory-p directory) (add-to-list (quote load-path) directory t))))' \
  -l configs/doom/tests/pilish-config-test.el -f ert-run-tests-batch-and-exit
```

## Doom minibuffer editing

- **Tab** inserts the highlighted Vertico candidate (the first suggestion by
  default), without accepting the prompt. File and project-file candidates are
  completed one path component at a time: `src/co` → `src/components/` →
  `src/components/button.el`, rather than inserting the whole nested path at once.
  Command and buffer candidates are inserted in full.
- **Backspace** deletes one character, including immediately after `/`; it does
  not remove the entire directory name. Both GUI and terminal key events are
  covered.
- **Enter** still accepts a candidate. **C-j/C-k**, the arrow keys, and Shift-Tab
  retain their existing candidate-navigation behavior.

These bindings live in `configs/doom/vertico-navigation.el`, loaded after Doom's
Vertico defaults. They do not change the separate Gloom configuration. No
`doom sync` or Nix activation is needed; reload Doom or restart Emacs.

Regression tests, using Doom's already installed Vertico and Compat packages:

```sh
build="$HOME/.local/share/doom-emacs/.local/straight/build-$(/opt/homebrew/bin/emacs -Q --batch --eval '(princ emacs-version)')"
/opt/homebrew/bin/emacs -Q --batch -L "$build/compat" -L "$build/vertico" \
  -l configs/doom/tests/vertico-navigation-test.el -f ert-run-tests-batch-and-exit
```

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
