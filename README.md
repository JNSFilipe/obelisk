# Obelisk

Declarative macOS system configuration.
[nix-darwin](https://github.com/nix-darwin/nix-darwin) manages the system,
[home-manager](https://github.com/nix-community/home-manager) manages the user environment,
and [homebrew](https://brew.sh) (via nix-darwin) handles GUI apps not packaged through Nix.

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
  brew-upgrade    Explicitly update and upgrade declared Homebrew packages
  build           Build without activating (dry run)
  check           Build the system and run flake checks
  diff            Show what changed between current and built config
  doom-update     Update the pinned Nix Doom Emacs inputs
  gc              Garbage-collect old nix store paths (keeps 7 days)
  gc-all          Garbage-collect ALL unused nix store paths
  generations     List all system generations
  packages        List all nix-managed packages in the current profile
  rollback        Roll back to the previous generation
  store-size      Show nix store disk usage
  switch          Build and activate the system configuration
  uninstall-nix   Completely remove nix from the system
  update          Update all flake inputs (nixpkgs, home-manager, nix-darwin)
  upgrade         Update inputs and activate in one step
```

## Workflows

**Edit nix config and apply:**
```bash
vim nix/packages.nix   # or darwin.nix, homebrew.nix, home.nix
make switch
```

**Edit app config (live symlink):**
```bash
vim configs/ghostty/config   # takes effect immediately, no rebuild needed
```

**Add a new CLI tool:**
```bash
vim nix/packages.nix         # add package name
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

**Update everything (nixpkgs, home-manager, nix-darwin) and activate:**
```bash
make upgrade
```

Homebrew deliberately does not update during system activation. Upgrade its
declared formulae and casks separately:
```bash
make brew-upgrade
```

**Update Doom Emacs:**
```bash
make doom-update             # update Unstraightened and its Doom inputs
make switch                  # rebuild Doom and activate it
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
flake.nix                  Entry point
nix/
  darwin.nix               System: nix settings, macOS defaults, users
  homebrew.nix             GUI casks + CLI exceptions not in nixpkgs
  home.nix                 User: zsh, programs, config symlinks
  packages.nix             CLI packages from nixpkgs
configs/
  tmux.conf                tmux settings (loaded via programs.tmux)
  doom/                    Doom Emacs user config
  emacs/                   Legacy Doom checkout (no longer linked or executed)
  vemacs/                  Vanilla Emacs config
  nvim/                    Neovim
  zed/                     Zed
  kitty/                   Kitty theme files
  wezterm/                 WezTerm terminal
  kanata/                  Physical keyboard remapping
  hammerspoon/             App-aware GUI keybindings
  scripts/                 Shell scripts (zzz, ttt, fzflauncher, ...)
  wallpapers/              Desktop backgrounds
```

Configs managed declaratively by home-manager modules (no files in `configs/`):
atuin, ghostty, helix, lazygit, yazi, git, fzf, zoxide, tmux plugins.

## Where packages live

| What | Where | Why |
|------|-------|-----|
| CLI tools | `nix/packages.nix` | Reproducible, pinned via flake.lock |
| Nix GUI apps | `nix/darwin.nix` `environment.systemPackages` | Copied into `/Applications/Nix Apps` |
| Other GUI apps | `nix/homebrew.nix` casks | macOS apps not managed through Nix |
| CLI not in nixpkgs | `nix/homebrew.nix` brews | Currently `herdr` |
| Shell / tool config | `nix/home.nix` `programs.*` | Declarative, managed by home-manager |
| App configs (no module) | `configs/` | Symlinked live into `~/.config/` |

Editing most files under `configs/` takes effect immediately (live symlinks).
`configs/doom/` is bundled into the Nix Doom package and requires `make switch`.
Editing `nix/` or `programs.*` settings requires `make switch` to apply.

System activation installs missing Homebrew packages but does not update or
remove packages. It fails with a drift report when an undeclared package is
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

## Managing nix-darwin services

Services enabled via `services.<name>.enable = true` in `darwin.nix` (e.g. Tailscale)
are managed by `launchctl` on macOS.

```bash
# List all nix-managed daemons
sudo launchctl list | grep org.nix

# Check status of a specific service (e.g. Tailscale)
sudo launchctl print system/org.nixos.tailscaled

# Restart a service
sudo launchctl kickstart -k system/org.nixos.tailscaled

# Stop a service (modern / legacy)
sudo launchctl bootout system/org.nixos.tailscaled
sudo launchctl stop org.nixos.tailscaled

# Start a stopped service (modern / legacy)
sudo launchctl bootstrap system /Library/LaunchDaemons/org.nixos.tailscaled.plist
sudo launchctl start org.nixos.tailscaled
```

The service name pattern is `org.nixos.<daemon>`. To find the exact name:
```bash
ls /Library/LaunchDaemons/org.nixos.*
```

## Adding a new tool

1. **CLI tool in nixpkgs**: add to `nix/packages.nix`, run `make switch`
2. **GUI app**: add cask name to `nix/homebrew.nix`, run `make switch`
3. **Config file**: add to `configs/`, add `home.file` entry in `nix/home.nix`, run `make switch`

## Uninstall

```bash
make uninstall-nix
```

Removes nix, the daemon, and all store paths. Homebrew and GUI apps are untouched.
