# Obelisk Dotfiles

A comprehensive macOS dotfiles configuration featuring modern development tools, editors, and terminal setup optimized for productivity.

## =€ Quick Start

### Prerequisites

- macOS (primary target platform)
- Git
- Homebrew (will be used for package installation)

### Installation

**Important:** This repository uses Git submodules. Always clone with the `--recurse-submodules` flag:

```bash
git clone --recurse-submodules https://github.com/jfilipe/obelisk.git ~/.dotfiles
cd ~/.dotfiles
```

If you already cloned without submodules, initialize them:

```bash
git submodule update --init --recursive
```

### Setting Up the Environment

1. **Install Homebrew packages:**
   ```bash
   brew bundle --file=obelisk/dot-config/Brewfile
   ```

2. **Deploy configurations using GNU Stow:**
   ```bash
   cd obelisk
   stow dot-config dot-tmux.conf dot-zshrc
   ```

## =Á Configuration Overview

### Core Configurations

- **Shell**: Zsh with modern enhancements
  - History optimization (10M entries)
  - Auto tmux session management
  - Fast package management with `znap`
  - Fish-like autocompletion
  - fzf integration

- **Terminal Multiplexer**: tmux
  - Custom key bindings (prefix: `Ctrl-z`)
  - Vim-like pane navigation
  - Catppuccin theme with Oxocarbon color scheme
  - Smart editor integration (Vim/Emacs/Neovim)

- **Package Management**: Homebrew
  - Comprehensive package list in `Brewfile`
  - Development tools, editors, fonts, and utilities

### Editors & Development

- **Doom Emacs**: Modern Emacs configuration (submodule)
- **Neovim**: Advanced Vim configuration
- **Zed**: Lightning-fast code editor
- **Visual Studio Code**: Cross-platform development

### Terminal Applications

- **Ghostty**: Modern terminal emulator
- **tmux**: Terminal multiplexer with plugins
- **Starship**: Cross-shell prompt
- **fzf**: Fuzzy finder
- **ripgrep**: Fast text search
- **lazygit**: Git TUI

### Utilities & Scripts

Located in `dot-config/scripts/`:
- `zzz`: tmux sessionizer
- `ttt`: secondary tmux tool
- `fzflauncher`: fzf-based application launcher
- `preview`: File preview utility
- Various connection and utility scripts

## =' tmux Setup

### Installing TPM (Tmux Plugin Manager)

The tmux configuration uses several plugins managed by TPM. Install it:

```bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

### Installing tmux Plugins

After installing TPM, launch tmux and install plugins:

```bash
tmux
# Inside tmux, press: Ctrl-z + I (capital i)
```

### Key tmux Plugins

- **tpm**: Plugin manager
- **tmux-navigate**: Smart pane navigation
- **tmux-yank**: System clipboard integration
- **catppuccin/tmux**: Beautiful theme

### tmux Key Bindings

- **Prefix**: `Ctrl-z` (instead of default `Ctrl-b`)
- **Pane Navigation**: `Ctrl-h/j/k/l` (Vim-style)
- **Split Panes**: `prefix + s` (horizontal), `prefix + S` (vertical)
- **Sessionizer**: `prefix + Space` (launches custom script)
- **Reload Config**: `prefix + r`

## =æ Package Management

### Homebrew Integration

Install all packages at once:

```bash
brew bundle --file=obelisk/dot-config/Brewfile
```

Key categories:
- **Terminal Tools**: tmux, fzf, ripgrep, starship
- **Editors**: Emacs, Neovim, Zed, VS Code
- **Languages**: Python, Rust, Go, Node.js
- **Fonts**: Nerd Fonts collection
- **Development**: Git tools, LaTeX, various SDKs

## = Git Submodules

### Updating Doom Emacs

The Doom Emacs configuration is included as a Git submodule. To update it:

```bash
git submodule update --remote
```

This will pull the latest changes from the upstream Doom Emacs repository.

### Working with Submodules

- **Check submodule status**: `git submodule status`
- **Update all submodules**: `git submodule update --remote --recursive`
- **Pull changes including submodules**: `git pull --recurse-submodules`

## <¨ Theming

The configuration uses a consistent Oxocarbon-inspired color scheme across:
- tmux status bar
- Terminal applications
- Editor themes (where available)

## =à Customization

### Adding New Configurations

1. Place new config files in `obelisk/dot-config/`
2. Use the `dot-` prefix for dotfiles (handled by Stow)
3. Update the Brewfile for new packages
4. Test with `stow --dry-run` before applying

### Modifying Existing Configs

All configurations are organized by application in `dot-config/`. Edit the relevant files and re-run `stow` to apply changes.

## = Troubleshooting

### Common Issues

1. **Submodules not found**: Ensure you cloned with `--recurse-submodules`
2. **tmux plugins not working**: Install TPM and run `prefix + I`
3. **Zsh completions slow**: Rebuild completions with `rm ~/.zcompdump && compinit`
4. **Stow conflicts**: Remove existing dotfiles or use `stow --adopt`

### Dependencies

Some configurations assume specific directory structures:
- Scripts expect `~/.config/scripts/` to be in PATH
- tmux sessionizer scripts require certain project directory layouts
- Emacs configuration may need initial setup after first launch

## =Ä License

This configuration is provided as-is for personal use and customization.

