# Obelisk

Declarative macOS system configuration.
[nix-darwin](https://github.com/nix-darwin/nix-darwin) manages the system,
[home-manager](https://github.com/nix-community/home-manager) manages the user environment,
and [homebrew](https://brew.sh) (via nix-darwin) handles GUI apps.

## Bootstrap

```bash
# Clone (Doom Emacs is a submodule)
git clone --recurse-submodules https://github.com/jfilipe/obelisk.git
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
  brew-orphans    List homebrew formulae not declared in homebrew.nix
  build           Build without activating (dry run)
  check           Evaluate the flake and run checks (no build)
  diff            Show what changed between current and built config
  doom-update     Pull latest Doom Emacs
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

**Update Doom Emacs:**
```bash
make doom-update             # pull latest Doom
doom sync                    # rebuild packages
```

**Safe test before applying:**
```bash
make build                   # build without activating
make diff                    # inspect what changed
make switch                  # apply if happy
```

**Something broke after switch:**
```bash
make rollback                # instant revert to previous generation
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
flake.nix                  Entry point
nix/
  darwin.nix               System: nix settings, macOS defaults, users
  homebrew.nix             GUI casks + CLI exceptions not in nixpkgs
  home.nix                 User: zsh, programs, config symlinks
  packages.nix             CLI packages from nixpkgs
configs/
  tmux.conf                tmux settings (loaded via programs.tmux)
  doom/                    Doom Emacs user config
  emacs/                   Doom Emacs install (git submodule)
  vemacs/                  Vanilla Emacs config
  nvim/                    Neovim
  zed/                     Zed
  kitty/                   Kitty theme files
  wezterm/                 WezTerm terminal
  karabiner/               Key remapping
  kanata/                  Keyboard remapper
  scripts/                 Shell scripts (zzz, ttt, fzflauncher, ...)
  wallpapers/              Desktop backgrounds
```

Configs managed declaratively by home-manager modules (no files in `configs/`):
atuin, ghostty, helix, lazygit, yazi, git, fzf, zoxide, tmux plugins.

## Where packages live

| What | Where | Why |
|------|-------|-----|
| CLI tools | `nix/packages.nix` | Reproducible, pinned via flake.lock |
| GUI apps | `nix/homebrew.nix` casks | macOS .app bundles, no nix equivalent |
| CLI not in nixpkgs | `nix/homebrew.nix` brews | `doxx`, `codex-acp`, `ty` |
| Shell / tool config | `nix/home.nix` `programs.*` | Declarative, managed by home-manager |
| App configs (no module) | `configs/` | Symlinked live into `~/.config/` |

Editing files under `configs/` takes effect immediately (live symlinks).
Editing `nix/` or `programs.*` settings requires `make switch` to apply.

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
