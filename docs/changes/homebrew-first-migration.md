# Homebrew-first package management

## Scope

Invert the package policy: Homebrew (`nix/homebrew.nix`) becomes the source for
every tool that homebrew-core provides. `nix/packages.nix` keeps only what brew
cannot supply. nix-darwin + home-manager stay in charge of the system,
dotfiles, and `programs.*` modules (git, tmux, zsh, fzf, atuin, zoxide, kitty,
ghostty, helix, lazygit, yazi, doom-emacs) — those are out of scope.

Confirmed with the user:
- migrate GNU tools even though brew names them `gls`/`gmake`
- migrate compilers, runtimes and C/C++ libraries too
- move the AI CLIs (`claude-code`, `codex`) back to brew casks

## Findings

Every entry of `packages.nix` exists in homebrew-core (verified via
`brew info --formula`), with these name changes:

| nixpkgs | brew |
| --- | --- |
| `gnumake` | `make` (GNU binary is `gmake`) |
| `pkg-config` | `pkgconf` (ships a `pkg-config` shim) |
| `kubectl` | `kubernetes-cli` |
| `postgresql` | `postgresql@18` (keg-only) |
| `awscli2` | `awscli` |
| `python313` | `python@3.13` |
| `nodejs` | `node` |
| `libusb1` | `libusb` |
| `python3Packages.{pygments,python-lsp-server,pybind11}` | `pygments`, `python-lsp-server`, `pybind11` |
| `claude-code`, `codex` | brew **casks** (no formula) |

Keg-only formulae need explicit PATH entries: `llvm`, `postgresql@18`,
`rustup`. `openblas` is keg-only but library-only (pkg-config path only).

Dropped with no brew equivalent: `aspellDicts.en`, `aspellDicts.en-computers`
— homebrew's `aspell` bundles its own dictionaries, and nix-store dicts are
invisible to a brew-built aspell.

## Tasks

- [x] Move every migratable package from `packages.nix` into `homebrew.nix`
      (`brews` for formulae, `casks` for `claude-code` + `codex`)
- [x] Reduce `packages.nix` to a documented stub (escape hatch for tools brew lacks)
- [x] Add keg-only PATH entries in `darwin.nix` and refresh its brew comment
- [x] Update README + Makefile help so the documented workflow is brew-first
- [x] Verify: module evaluation, deadnix, statix, nixfmt
- [x] `nix flake check` → all checks passed (system closure builds)
- [ ] `make switch` (user-run: installs the brew packages, drops the nix ones)

## Evidence

- `brew info --formula <name>` for all 57 target formulae: every one resolves in
  homebrew-core; `claude-code` and `codex` resolve as casks only.
- `nix eval .#darwinConfigurations.gauss.config.homebrew.brews` → 57 entries,
  matching the migrated list.
- `nix eval … config.homebrew.casks` → includes `claude-code`, `codex`
  alongside the pre-existing `claude` and `codex-app` desktop apps.
- `nix eval … home-manager.users.jfilipe.home.packages` → 17 packages, all
  contributed by `programs.*` modules; `packages.nix` itself now adds none.
- `nix eval … config.environment.systemPath` →
  `…:/opt/homebrew/bin:/opt/homebrew/sbin:/opt/homebrew/opt/llvm/bin:/opt/homebrew/opt/rustup/bin:/opt/homebrew/opt/postgresql@18/bin:…`
- `deadnix --fail flake.nix nix` → clean.
- `statix check` (repo config, flake.nix + nix/) → clean.
- `nixfmt --check flake.nix nix/*.nix` → clean.
- `nix flake check` first failed with
  `error: 'emacs30' has been superseded by 'emacs' on august 2026` — an
  independent break from the uncommitted `flake.lock` bump, hit by the
  `vanilla-emacs` wrapper in `home.nix`. Against the locked nixpkgs,
  `emacs30-macport` (Doom) still resolves to 30.2.50 while `emacs30` is gone
  and `emacs` is 31.1. The wrapper now uses `pkgs.emacs`, after which
  `nix flake check` reports `all checks passed!` — full system closure built,
  deadnix and statix clean. (An earlier run looked green only because `| tail`
  masked nix's exit code.)
- `brew bundle cleanup` against the newly built Brewfile
  (`/nix/store/x8i59bfjw1n5jdamz50v5iz3644g2fl3-Brewfile`, 59 brews + 47 casks)
  reports no packages to uninstall: zero drift once activated.

## Follow-ups landed after the migration

- Hammerspoon removed entirely: `configs/hammerspoon/` (tracked `init.lua`,
  untracked `Spoons/`) deleted and the README structure line dropped. It had no
  nix entry, no cask, and the app was not installed.
- `make switch` is now the single up-to-date command: it depends on `update`
  (flake inputs), and `homebrew.onActivation` sets `autoUpdate = true` and
  `upgrade = true` so activation runs `brew update` and upgrades outdated
  declared packages. `upgrade` became an alias for `switch`.
- `make switch` finishes with `$(MAKE) gc` (`nix-collect-garbage
  --delete-older-than 7d`), so each update's orphaned store paths are reclaimed
  in the same command. The 7-day window keeps the replaced generation
  rollbackable. `make -n switch` confirms the order: flake update →
  `darwin-rebuild switch` → `make gc`.
- Added `make switch-locked` — activation with `HOMEBREW_BUNDLE_NO_UPGRADE=1
  HOMEBREW_NO_AUTO_UPDATE=1` and no flake update, for offline or config-only
  changes.
- `make brew-upgrade` now also runs `brew upgrade --cask --greedy`, since
  `brew bundle` has no `--greedy` and self-updating casks are skipped during
  activation.
- Emacs now runs as a login daemon: `services.emacs.enable = true` with
  `package = config.programs.doom-emacs.finalEmacsPackage` (the module's default
  is a bare `pkgs.emacs`, not the Doom bundle). home-manager emits
  `launchd.agents.emacs` — `emacs --fg-daemon`, `RunAtLoad = true`, restarted on
  crash but not after a clean `kill-emacs`. Because launchd hands agents a bare
  PATH and Doom shells out to rg/fd/git — all brew-provided now — the agent gets
  an explicit `EnvironmentVariables.PATH`; `home.sessionPath` could not be
  reused since its entries contain a literal `$HOME` that launchd will not
  expand. `nix flake check` → all checks passed.
- Daemon ergonomics: `ec` / `et` aliases (GUI / terminal frame) plus
  `EDITOR = emacsclient -t -a ''` and `VISUAL = emacsclient -c -a ''`. The
  `-a ''` fallback starts the daemon if it is not running. Verified in the built
  generation: `.zshrc` renders `alias -- ec='emacsclient -c -a '\'''\'''`,
  `hm-session-vars.sh` renders `export EDITOR="emacsclient -t -a ''"`, and
  `LaunchAgents/org.nix-community.home.emacs.plist` wraps the daemon as
  `/bin/wait4path /nix/store && exec …/bin/emacs --fg-daemon` with the PATH
  dict, `RunAtLoad`, and the crash-only `KeepAlive`.
- Browsers: `firefox` dropped (cask uninstalled; its `~/Library` profile data
  left untouched) and replaced by `orion`, installed and declared.
- The 7 cask upgrades that failed during activation failed on disk space, not
  configuration: a cask upgrade backs the old app up into the Caskroom before
  installing the new one, so it needs download + old + new at once, and the
  volume was at 98%. Re-run individually after clearing the brew cache, `zed`,
  `warp` and `datagrip` all upgraded cleanly. `helium-browser`, `chatgpt` and
  `whatsapp` are still outdated because they were running; upgrading them
  terminates the app, so it was left to the user.
- Verified: `make -n switch` shows `nix flake update` then `darwin-rebuild
  switch`; `onActivation` evaluates to `autoUpdate=true upgrade=true
  cleanup=check`; nixfmt and deadnix clean.

## Next

`make switch` (user-run). It installs ~50 formulae plus the `claude-code` and
`codex` casks; the nix copies leave the profile immediately and the store on
the next `make gc`.

Previously-undeclared brew packages: `cloudflared` (brews) and `nextcloud`
(casks) are now declared. `tailscale` is declared too, at the user's
request. The daemon stays nix-owned (`services.tailscale`); the brew formula
only duplicates the CLI, which loses on PATH order. Moving daemon ownership to
brew would mean disabling `services.tailscale` and running
`sudo brew services start tailscale` — not done, and not declarative.

Also fixed here: the zsh `ls` aliases passed `--color=auto`, which only GNU ls
accepts. With coreutils coming from brew (g-prefixed), they now call `gls`.
