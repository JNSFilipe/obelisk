# Obelisk

Declarative macOS system configuration.
[nix-darwin](https://github.com/nix-darwin/nix-darwin) manages the system,
[home-manager](https://github.com/nix-community/home-manager) manages the user environment,
and [homebrew](https://brew.sh) (via nix-darwin) is the default package source for
CLI tools and GUI apps alike.

## Bootstrap

```bash
# Clone (the legacy Emacs checkout is an optional submodule)
git clone https://github.com/jfilipe/obelisk.git
cd obelisk

# Install Nix (Determinate Systems installer)
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# First-time activation
make bootstrap
```

## Usage

```
make help
```

```
  bootstrap       First-time nix-darwin install (run after installing nix)
  brew-orphans    List Homebrew packages not declared in homebrew.nix
  brew-upgrade    Upgrade Homebrew, including casks that self-update (--greedy)
  build           Build without activating (dry run)
  check           Build the system and run flake checks
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
brew info --formula NAME     # confirm homebrew-core has it
vim nix/homebrew.nix         # add to brews (or nix/packages.nix if brew lacks it)
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
| Nix GUI apps | `nix/darwin.nix` `environment.systemPackages` | Copied into `/Applications/Nix Apps` |
| CLI not in homebrew-core | `nix/packages.nix` | Escape hatch; currently empty |
| Shell / tool config | `nix/home.nix` `programs.*` | Declarative, managed by home-manager |
| App configs (no module) | `configs/` | Symlinked live into `~/.config/` |

Editing most files under `configs/` takes effect immediately (live symlinks).
`configs/doom/` is linked live. Changes to `init.el` or `packages.el` require
`make doom-sync` and an Emacs restart; ordinary `config.el` edits require a reload
or restart. See [Emacs installation](docs/emacs.md) for setup and maintenance.
Editing `nix/` or `programs.*` settings requires `make switch` to apply.

Locked activation installs missing Homebrew packages but does not upgrade or
remove packages. Regular `make switch` also updates and upgrades them.
Activation fails with a drift report when an undeclared package is
present; use `make brew-orphans` to inspect the same report beforehand.

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
