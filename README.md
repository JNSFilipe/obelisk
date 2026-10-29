# Obelisk

Declarative macOS system configuration.
[nix-darwin](https://github.com/nix-darwin/nix-darwin) manages the system,
[home-manager](https://github.com/nix-community/home-manager) manages the user environment,
and [homebrew](https://brew.sh) (via nix-darwin) supplies every package it offers
as a formula or cask. Nix packages are used only when Homebrew cannot supply them.

## Bootstrap

```bash
# Clone (the legacy Emacs checkout is an optional submodule)
git clone https://github.com/jfilipe/obelisk.git
cd obelisk

# Install Homebrew first: https://brew.sh (Apple Silicon: /opt/homebrew)
# Then install Nix (Determinate Systems installer)
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# First-time activation (installs Homebrew shells before changing the login shell)
make bootstrap
```

## Usage

```
make help
```

```
  bootstrap       First-time nix-darwin install (run after installing Homebrew and Nix)
  brew-orphans    List Homebrew packages not declared in homebrew.nix
  brew-upgrade    Upgrade Homebrew, including casks that self-update (--greedy)
  build           Build without activating (dry run)
  check           Build the system and run flake checks
  check-runtime   Test generated shells using installed Homebrew tools (isolated state)
  diff            Show what changed between current and built config
  doom-install    Bootstrap writable Doom with Homebrew Emacs
  doom-sync       Sync Doom modules/packages after configuration changes
  doom-update     Upgrade conventional Doom and its packages
  gc-all          Garbage-collect ALL unused nix store paths
  gc              Garbage-collect old nix store paths (keeps 7 days)
  generations     List all system generations
  help            Show this help
  packages        List all nix-managed packages in the current profile
  rollback        Roll back Nix system and Home Manager state
  store-size      Show nix store disk usage
  switch-locked   Activate without updating anything (pinned flake.lock, no brew upgrade)
  switch          Update all inputs, activate, then garbage-collect
  uninstall-nix   Completely remove nix from the system
  update          Update all flake inputs (nixpkgs, home-manager, nix-darwin)
  upgrade         Alias for switch, which already updates everything
```

## Workflows

**Edit nix config and apply:**
```bash
vim nix/homebrew.nix   # or darwin.nix, home.nix, packages.nix
make switch
```

**Edit app config (live symlink):**
```bash
vim configs/ghostty/config   # takes effect immediately, no rebuild needed
```

**Add a new CLI tool:**
```bash
brew info --formula NAME || brew info --cask NAME  # check both distributions
vim nix/homebrew.nix         # add to brews/casks; Nix only if neither exists
make switch
```

**Add a new GUI app:**
```bash
vim nix/homebrew.nix         # add cask name
make switch
```

**Add a new config file:**
```bash
cp myconfig configs/myapp/   # add config files
vim nix/home.nix             # add home.file entry with mkOutOfStoreSymlink
make switch
```

**Update everything and activate:**
```bash
make switch
```

`make switch` is the single up-to-date command: it updates every flake input
(nixpkgs, home-manager, nix-darwin), activation runs `brew update` and
upgrades outdated declared formulae and casks, and it finishes with `make gc`
so the store paths the update orphaned do not pile up. The GC keeps 7 days, so
the generation the switch replaced stays available to `make rollback`.
`make upgrade` is an alias.

To activate the pinned configuration without updating anything — offline, or
when you only changed a `nix/` file:
```bash
make switch-locked
```

Casks that ship their own updater are skipped during activation, because that
needs `brew upgrade --greedy` and `brew bundle` does not accept it. Sweep them
explicitly:
```bash
make brew-upgrade            # brew update + bundle install + --greedy cask sweep
```

**Update Doom Emacs:**
```bash
make doom-update             # Doom upgrade, using Homebrew Emacs
# Save buffers and restart Emacs afterward.
```

**Safe test before applying:**
```bash
make build                   # build without activating
make diff                    # inspect what changed
make switch                  # apply if happy
```

**Something broke after switch:**
```bash
make rollback                # revert Nix-managed system and Home Manager state
```

Rollback does not revert Homebrew upgrades or files exposed through the live
`configs/` symlinks. Both are intentionally managed outside Nix generations.

**Atuin reports a missing migration:**

This usually means an older Atuin is opening a database already migrated by a
newer one. Do **not** delete `history.db` or rows from `_sqlx_migrations`.
Homebrew owns Atuin here. Home Manager renders its settings, and both Zsh and
Nushell initialize the Homebrew executable. Do not install another Atuin with Nix.

```bash
which -a atuin
atuin --version
# If /opt/homebrew/bin/atuin also exists:
/opt/homebrew/bin/atuin --version
```

Upgrade the Homebrew version rather than downgrading the database:

```bash
brew upgrade atuin
make switch-locked             # remove any legacy Nix-installed copy via activation
exec /opt/homebrew/bin/zsh -l   # reload integrations and command lookup
```

The first Homebrew-only migration also works with `make switch-locked`: it
installs missing declared packages but does not upgrade existing ones. Old Nix
generations may still contain incompatible Atuin binaries; avoid rolling back
to them. Nix rollback does not roll back `~/.local/share/atuin`.

Zsh creates `${XDG_DATA_HOME:-$HOME/.local/share}/zsh` before loading plugins,
so recent-directory history also works on a fresh home or after state cleanup.
For an already-running shell with a missing directory, create it once:

```bash
mkdir -p -- "${XDG_DATA_HOME:-$HOME/.local/share}/zsh"
```

**Reclaim disk space:**
```bash
make gc                      # remove store paths older than 7 days
make gc-all                  # aggressive: remove all unused paths
make store-size              # check how much space nix uses
```

**Audit homebrew drift:**
```bash
make brew-orphans            # list formulae not declared in homebrew.nix
```

## Structure

```
flake.nix                  Entry point; the host list lives here
nix/
  darwin.nix               System: nix settings, macOS defaults, users
  homebrew.nix             Default package source: brew formulae + casks
  home.nix                 User: zsh, programs, config symlinks
  packages.nix             nixpkgs escape hatch (things brew does not ship)
  nushell.nix              Nushell, alongside zsh (zsh stays the login shell)
  hosts/
    gauss.nix              Personal Mac: machine-specific settings only
    thorp.nix              Work Mac: machine-specific settings only
configs/
  tmux.conf                tmux settings (loaded via programs.tmux)
  nushell/                 Nushell prompt and commands (loaded via programs.nushell)
  omniwm/                  OmniWM tiling window manager settings
  doom/                    Doom Emacs user config
  gloom/                   Plain-Elisp Emacs 30+ config (linked as gloom-emacs)
  vemacs/                  Vanilla Emacs config
  nvim/                    Neovim
  zed/                     Zed
  kitty/                   Kitty theme files
  wezterm/                 WezTerm terminal
  scripts/                 Shell scripts (zzz, ttt, fzflauncher, ...)
  wallpapers/              Desktop backgrounds
```

Configs managed declaratively by home-manager modules (no files in `configs/`):
atuin, ghostty, helix, lazygit, yazi, git, fzf, zoxide, carapace, tmux plugins.

## Machines

Two Macs are managed from this repository:

| Host | Profile | Role |
|------|---------|------|
| `gauss` | `personal` | Personal machine |
| `thorp` | `work` | Work machine |

`make` detects which one it is running on from the local hostname, so the same
commands work on both. Build the other machine's configuration from either one
with `make build HOST=thorp`.

What differs is deliberately small:

- **Homebrew packages.** `nix/homebrew.nix` has a shared list plus
  `personalOnly` and `workOnly` groups selected by the host's profile. Today
  only personal apps are split out (leisure, home hardware, personal accounts);
  every CLI tool is shared, and the work-only group is still empty.
- **Machine-specific settings.** `nix/hosts/<host>.nix` is the place for macOS
  defaults, launchd agents or paths that exist on one machine only.
  `system.stateVersion` lives there because it is per-install by definition.

Everything else — dotfiles, shells, editors, git identity — is shared on
purpose. Adding a third Mac is one line in `flake.nix` plus a file in
`nix/hosts/`.

`nix flake check` builds **both** configurations, so a change cannot quietly
break the machine that is not in front of you.

## Where packages live

| What | Where | Why |
|------|-------|-----|
| CLI tools | `nix/homebrew.nix` brews | Default source; one upgrade path for everything |
| GUI apps | `nix/homebrew.nix` casks | Plus CLIs shipped only as casks (`claude-code`, `codex`) |
| Packages unavailable in Homebrew | `nix/packages.nix` or the relevant Nix module | Only after checking formulae and casks; currently tmux plugins and `antigravity-acp` (Gloom's Antigravity agent), allowlisted by name in `flake.nix` |
| Shell / tool config | `nix/home.nix` `programs.*` or generated files | Configuration only; never a reason to install a second binary |
| App configs (no module) | `configs/` | Symlinked live into `~/.config/` |

Pi (`pi-coding-agent` formula), Zen Browser (`zen` cask), Claude Code
(`claude-code` cask), OmniWM, Purge, and `tailscale-app` are common to
all machines, not personal/work-only packages. The flake check enforces this
shared baseline and excludes the conflicting `claude-code@latest` cask.

This policy includes shells, plugins, fonts, and packages normally pulled in by
Home Manager. Modules use `package = null` where supported. Atuin, zoxide, fzf,
carapace, and Opam modules require a Nix binary, so their configuration and
integrations are managed separately instead. Zsh initializes tools from stable
`/opt/homebrew` paths. Nushell refreshes its generated integration files in
`~/.cache/obelisk/nushell` during `env.nu`, before `config.nu` is parsed, so
Homebrew upgrades do not leave stale hooks. Application data stays in place.
The login shell is `/opt/homebrew/bin/zsh`; Kitty is `/Applications/kitty.app`.

Nix still supplies its own management tools, custom configuration wrappers, and
isolated build/check dependencies. These are not duplicate user applications.
`nix flake check` audits the declared home and system package lists on both
hosts against the small Nix-only/configuration allowlist. After installing the
Homebrew packages, `make check-runtime` smoke-tests the generated Zsh/Nushell
integrations and tmux using isolated state. It also checks Atuin against an
online SQLite backup of existing history, never against the live database.

Editing most files under `configs/` takes effect immediately (live symlinks).
`configs/doom/` is linked live. Changes to `init.el` or `packages.el` require
`make doom-sync` and an Emacs restart; ordinary `config.el` edits require a reload
or restart. See [Emacs installation](docs/emacs.md) for setup and maintenance.
Editing `nix/` or `programs.*` settings requires `make switch` to apply.

Locked activation installs missing Homebrew packages but does not upgrade or
remove packages. Regular `make switch` also updates and upgrades them.
Activation fails with a drift report when an undeclared package is
present; use `make brew-orphans` to inspect the same report beforehand.

## Pi extensions and skills

Pi itself is installed by Homebrew. `nix/pi.nix` declares all installed Pi
extension/skill packages for every machine:

- `npm:pi-web-access`
- `npm:@juicesharp/rpiv-ask-user-question`
- `npm:@heyhuynhgiabuu/pi-task` (including its bundled `task` skill)
- `git:github.com/JNSFilipe/jig` (all 11 Jig skills)
- `npm:pi-claude-bridge`

It also declares the default model, `claude-bridge/claude-opus-5-5` with `high`
thinking. Each of `defaultProvider`, `defaultModel`, and `defaultThinkingLevel`
is written only when unset, so a model chosen in Pi persists across activations.

Home Manager activation atomically replaces the `packages` key in
`~/.pi/agent/settings.json` and fills those unset defaults. Other preferences
and device identity stay local, and the file remains writable by Pi. Credentials, sessions, and installation
caches are not managed or copied into the repository. Invalid JSON, a busy
settings lock, or a symlink settings file stops activation instead of overwriting
it; resolve the reported issue and retry.

- `make switch` syncs the list, then runs the Homebrew Pi's
  `pi update --extensions --no-approve` to install/update global extensions.
  Project package declarations are ignored. Update failures stop activation.
- `make switch-locked` syncs the list without installing/upgrading packages;
  Pi installs missing packages on its next startup.

Pi owns the single installed Jig checkout at
`~/.pi/agent/git/github.com/JNSFilipe/jig`. After package reconciliation,
activation links its `skills/jig-*` directories into `~/.claude/skills` and
`~/.codex/skills`, so all three clients use the same files. It does not copy
skills into `~/.agents/skills`, which Pi also scans and would load twice.
Unrelated Claude plugins and other skills are not managed here.

Sharing preflights all destinations and refuses to overwrite unmanaged Jig
directories or foreign links; it also refuses leftover
`~/.agents/skills/jig-*` copies. On a machine with a manual Jig installation,
first install the Pi package, compare
and explicitly remove those manual copies (preserving any customizations),
then activate again. No routine activation recursively deletes skill files.
Only stale sharing links owned by this configuration are pruned. If a fresh
locked activation has no Jig checkout yet, sharing is deferred: start Pi to
install packages, then activate again, or use a regular `make switch`.

The list is authoritative: edit `nix/pi.nix` to add/remove a shared package.
Manual package-list changes are replaced on the next activation, but cached
packages removed from the list are not deleted. Versions are intentionally
unpinned and update independently of `flake.lock`.

`make check-pi` runs the flake checks (including isolated settings-merge and
skill-sharing tests), then tests generated activation for both hosts and locked
variants using a fake Pi, including dry-run behavior and shared skill links.
It never loads extensions or changes live settings.

## tmux

| Key | Action |
|-----|--------|
| `Ctrl-z` | Prefix (replaces Ctrl-b) |
| `Ctrl-h/j/k/l` | Pane navigation (passes through to vim/emacs/fzf) |
| `prefix + s` / `S` | Split horizontal / vertical |
| `prefix + Space` | Sessionizer (project picker) |
| `prefix + Ctrl-z` | Session switcher |
| `prefix + Tab` | Last window |
| `prefix + r` | Reload config |

## Tailscale

Homebrew's `tailscale-app` cask installs the standalone macOS app. The app
owns the VPN connection; `services.tailscale` is disabled and the separate
Homebrew `tailscale` formula is not installed.

`configs/scripts/tailscale` exposes the app's bundled CLI on the existing
scripts PATH, keeping CLI and app versions aligned:

```sh
open -a Tailscale
tailscale status
tailscale netcheck
```

Allow the app's network extension/VPN configuration when macOS prompts,
then sign in to the intended tailnet. This Mac uses the Digiwest Headscale
server with subnet routes enabled:

```sh
tailscale up --login-server http://tailscale.digiwest.pt:80 --accept-routes
```

If authentication is required, supply a current Headscale auth key separately;
keep credentials out of this repository. A hostname does not preserve a node's
identity when switching clients; verify the assigned address after migration.

Manage the connection through the app or `tailscale up` / `tailscale down`.
Do not start a separate `tailscaled` with `brew services` or `launchctl`.

## Adding a new tool

1. **CLI tool**: add to `brews` in `nix/homebrew.nix` (or `nix/packages.nix` if
   homebrew-core does not ship it), run `make switch`
2. **GUI app**: add cask name to `nix/homebrew.nix`, run `make switch`
3. **Config file**: add to `configs/`, add `home.file` entry in `nix/home.nix`, run `make switch`

## Uninstall

```bash
make uninstall-nix
```

Removes nix, the daemon, and all store paths. Homebrew and GUI apps are untouched.
