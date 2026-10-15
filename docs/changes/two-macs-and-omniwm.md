# Two Macs, OmniWM in, Raycast and Tangrid out

Status: built and verified, not activated (`make switch` needs the user's sudo
password, and one manual uninstall first — see Next)

## Why / scope
Two changes that happen to touch the same files:

1. Drop Raycast and Tangrid, declare OmniWM. OmniWM had already been installed
   by hand (`brew install --cask omniwm`, 0.7.5) and configured, but nothing in
   this repository knew about it.
2. Prepare the repository to manage a second Mac — `thorp`, the work machine —
   without forking it.

## Behavior

### Window management
- `raycast` removed from the cask list, along with its login agent
  (`launchd.agents.raycast`) and the activation script that claimed ⌘Space.
- `omniwm` added to the cask list, and `configs/omniwm/settings.toml` (a copy
  of the live 1105-line config) is linked to `~/.config/omniwm/settings.toml`.
  The link is file-level, not directory-level, so OmniWM keeps using
  `~/.config/omniwm` for anything else it writes. Like every other config here
  it is an out-of-store symlink, so OmniWM's own writes land in the repository.
- Tangrid was **not in the configuration at all** — only mentioned in prose in
  `docs/emacs.md` and in the migration change record, both now updated. The app
  itself is a manual `/Applications/Tangrid.app` install that Nix and Homebrew
  never managed, so removing it is a Finder drag, not a config change.

### ⌘Space
The old activation block did two things: point Raycast at ⌘Space, and disable
Spotlight's ⌘Space so Raycast could have it. Deleting the whole block would
have left ⌘Space bound to nothing, because the disabled hotkey persists in the
defaults database. The block is therefore replaced by one that re-enables
Spotlight's hotkey. Delete it if something else should own ⌘Space.

### Two machines
- `flake.nix` holds the host list: `gauss` → `personal`, `thorp` → `work`. It
  generates `darwinConfigurations.<host>` and `<host>-locked` for each, and
  threads `hostName`/`hostProfile` through `specialArgs` the way the repository
  already threads `userName` and `homeDirectory`.
- `nix/hosts/<host>.nix` is imported per machine and holds what is true of one
  machine only. `system.stateVersion` moved there from `nix/darwin.nix`, since
  it is per-install by definition and the two machines will diverge.
- `nix/homebrew.nix` takes `hostProfile` and gains `personalOnly`/`workOnly`
  selectors. Sixteen leisure, home-hardware and personal-account casks moved
  into the personal group; the work group is empty. Every formula stays shared,
  because every one of them is a CLI tool both machines want — the same hook is
  in place for brews when that stops being true.
- Identity stays shared on purpose: one `user.name`/`user.email` on both.
- `make` detects the host from `scutil --get LocalHostName`, so the same
  commands work on either machine; `make build HOST=thorp` targets the other.
- `nix flake check` now builds **both** configurations rather than one.

## Evidence
- `nix flake check`: all checks passed — two `darwin-system` derivations built
  plus deadnix and statix.
- Brewfile diff between the hosts, generated from the evaluated configs:
  gauss 119 entries, thorp 103, with exactly the sixteen intended casks
  gauss-only (adguard, bitwarden, chatgpt, gimp, hp-easy-start, iina,
  nextcloud, notion, notion-calendar, nvidia-geforce-now, paragon-extfs,
  raspberry-pi-imager, spotify, steam, tradingview, whatsapp) and nothing
  thorp-only.
- `raycast` appears in neither Brewfile; `omniwm` appears in both.
- The OmniWM link resolves to
  `/Users/jfilipe/Documents/GitHub/obelisk/configs/omniwm/settings.toml` on both
  hosts, so it is an out-of-store symlink and stays writable.
- `configs/omniwm/settings.toml` is byte-identical to the live file it was
  copied from, so activation replaces the real file with a symlink to the same
  content (home-manager backs the original up as `.bak-before-nix`).
- `make build` resolves to `#gauss` on this machine with no `HOST=` given.
- `nix fmt` touched only `flake.nix`, which this change rewrote.

## Next
Activation will **abort** until Raycast is uninstalled: `homebrew.onActivation
.cleanup = "check"` makes the activation script run `brew bundle cleanup`, which
errors on any installed-but-undeclared package, and Raycast is still installed.
Confirmed by reading the built activation script.

1. `brew uninstall --cask raycast`
2. `make switch` (or `make switch-locked`)
3. Optionally drag `/Applications/Tangrid.app` to the Trash — nothing manages it.

The personal/work cask split is a first pass and worth a review; moving a line
between the shared list and `personalOnly` is the whole edit. `thorp` has never
been activated — its `system.stateVersion` should be set to whatever nix-darwin
defaults to on the day that machine is first installed.
