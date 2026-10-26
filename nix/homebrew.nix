{ hostProfile, lib, ... }:

# ── nix-darwin homebrew integration ──────────────────────────────────────────
# Homebrew is the package source whenever it offers a formula or cask,
# CLI or GUI.  nix/packages.nix is the escape hatch for the few things
# brew cannot provide. nix-darwin/home-manager own system settings and
# dotfiles, not duplicate application packages. programs.* modules use
# package = null where supported; other integrations are configured directly.

let
  # Which machine gets what.  `hostProfile` comes from the host's entry in
  # flake.nix: gauss is "personal", thorp is "work".  Anything not wrapped in
  # one of these is installed on both.
  personalOnly = lib.optionals (hostProfile == "personal");
  workOnly = lib.optionals (hostProfile == "work");
in
{
  homebrew = {
    enable = true;
    taps = [
      "d12frosted/emacs-plus"
      "netbirdio/tap"
    ];

    onActivation = {
      # `make switch` is the one command that brings the machine up to date, so
      # activation runs `brew update` and upgrades outdated declared packages.
      # Casks that ship their own updater are left alone (that needs --greedy,
      # which `brew bundle` does not accept) — `make brew-upgrade` sweeps those.
      # Drift still aborts instead of deleting anything.
      autoUpdate = true;
      upgrade = true;
      cleanup = "check";
    };

    global = {
      autoUpdate = false;
      brewfile = true;
    };

    # ── Formulae (CLI) ───────────────────────────────────────────────────────
    # Keg-only formulae (llvm, postgresql@18, rustup) are not linked into
    # /opt/homebrew/bin; darwin.nix adds their opt/*/bin dirs to systemPath.

    brews = [
      # ── Shells, integrations and interactive tools ──────────────────────────
      "zsh"
      "bash"
      "nushell"
      "atuin"
      "zoxide"
      "fzf"
      "carapace"
      "tmux"
      "tuios"
      "yazi"
      "zsh-autocomplete"
      "zsh-autosuggestions"
      "fzf-tab"
      "pure"

      # ── Terminal utilities ─────────────────────────────────────────────────
      "coreutils" # GNU core utils, g-prefixed (gls, gdate, …)
      "moreutils" # sponge, vipe, ts, …
      "less"
      "man-db"
      "texinfo" # info viewer; nix-darwin's info module would install a Nix copy
      "wget"
      "htop"
      "nmap"
      "jq"
      "fd"
      "ripgrep"
      "aspell" # bundles its own dictionaries
      "doxx" # open word documents in terminal
      "xleak" # open excel documents in terminal
      "rtk" # token-saving proxy for common terminal tools
      "displayplacer" # sets display scaling; see configs/scripts/display-more-space
      "herdr"
      "mqttui"

      # ── Networking / tunnels ───────────────────────────────────────────────
      "cloudflared" # Cloudflare Tunnel client
      "netbirdio/tap/netbird" # NetBird VPN CLI
      "syncthing" # file sync; launchd agent and folders in nix/home.nix

      # ── Version control ────────────────────────────────────────────────────
      "git"
      "git-lfs"
      "lazygit"
      "gh"

      # ── SSH helpers ────────────────────────────────────────────────────────
      "autossh"
      "sshpass"

      # ── Serial / networking ────────────────────────────────────────────────
      "picocom"
      "portal" # file transfer to other devices

      # ── Build tools ────────────────────────────────────────────────────────
      "make" # GNU make as `gmake` (Apple's 3.81 stays as `make`)
      "cmake"
      "pkgconf" # provides the pkg-config shim
      "act" # run GitHub Actions locally

      # ── Syntax / parsing ───────────────────────────────────────────────────
      "tree-sitter"
      "pygments" # for syntax-highlighted previews

      # ── Editors (terminal) ─────────────────────────────────────────────────
      "neovim"
      "helix"

      # ── CI / containers / databases ────────────────────────────────────────
      # OrbStack provides the Docker engine, CLI, Compose, and buildx.
      "kubernetes-cli" # kubectl
      "opentofu" # infrastructure as code
      "postgresql@18" # Postgres tooling (keg-only)

      # ── AWS ────────────────────────────────────────────────────────────────
      "awscli"

      # ── Programming languages ──────────────────────────────────────────────
      # Python (use `uv` for project venvs; brew python for tooling)
      "python@3.13"
      "uv"
      "ty"
      "python-lsp-server"
      "ruff" # includes built-in LSP (replaces ruff-lsp)
      "black"

      # Rust (rustup owns the toolchain; keg-only, shims land in ~/.cargo/bin)
      "rustup"

      # Go
      "go"
      "wails"
      "gopls"

      # Node / JS
      "node"
      "bun"

      # Zig
      "zig"

      # Clojure
      "clojure"
      "opam" # OCaml package manager

      # C / C++
      "gcc"
      "libgccjit" # separate formula; brew's gcc is built without jit
      "llvm" # keg-only (macOS ships clang)
      "armadillo" # linear algebra
      "openblas" # keg-only, library-only: pass its lib/pkgconfig to builds
      "boost"
      "pybind11"
      "raylib"
      "libusb" # USB device access (needed for rpiboot, etc.)
      "rpiboot" # boot Raspberry Pi CM/Zero over USB

      # ── AI coding assistants ───────────────────────────────────────────────
      # claude-code and codex have no formula — see casks below.
      "codex-acp" # Codex Agent Client Protocol adapter
      "gemini-cli"
      "pi-coding-agent"
      "openspec"

      # ── LaTeX ──────────────────────────────────────────────────────────────
      "tectonic"
    ]
    ++ personalOnly [
      # Nothing personal-only yet: every formula above is a CLI tool both
      # machines want.
    ]
    ++ workOnly [
      # Nothing work-only yet.
    ];

    # ── Casks (GUI applications, plus CLIs shipped only as casks) ────────────

    casks = [
      # Standalone app owns the VPN; configs/scripts/tailscale uses its CLI.
      "tailscale-app"
      "netbirdio/tap/netbird-ui"

      # ── Terminals ──────────────────────────────────────────────────────────
      "kitty"
      # "cmux"
      # "wezterm"
      "ghostty"

      # ── Editors ────────────────────────────────────────────────────────────
      "d12frosted/emacs-plus/emacs-plus-app"
      "zed"
      "datagrip"
      "antigravity-cli"
      "sublime-text"
      # "visual-studio-code"

      # ── Version control ────────────────────────────────────────────────────
      "github" # GitHub Desktop

      # ── Window / keyboard management ──────────────────────────────────────
      "omniwm" # tiling WM; settings in configs/omniwm/settings.toml
      # "kindavim"
      # "dockdoor"

      # ── Browsers ───────────────────────────────────────────────────────────
      # Zen is the only declared browser; Safari ships with macOS.
      "zen" # Firefox-based; its profile is the one being synced between Macs

      # ── AI coding assistants (CLIs distributed only as casks) ──────────────
      "claude-code"
      "codex"

      # ── Fonts ──────────────────────────────────────────────────────────────
      # Fonts are Homebrew casks too, following the same ownership policy.
      "font-iosevka"
      "font-geist-mono"
      "font-source-code-pro"
      "font-fira-code-nerd-font"
      "font-fira-mono-nerd-font"
      "font-symbols-only-nerd-font"
      "font-jetbrains-mono-nerd-font"

      # ── LaTeX ──────────────────────────────────────────────────────────────
      # "mactex" # The all emcompasing, space eater approach
      # "basictex" # The lightweight alternative

      # ── Productivity ───────────────────────────────────────────────────────
      "claude"
      "codex-app"
      "shortcat"

      # ── Communication ──────────────────────────────────────────────────────
      "zoom"
      "microsoft-teams"

      # ── Microsoft Office ───────────────────────────────────────────────────
      "microsoft-word"
      "microsoft-excel"
      "microsoft-powerpoint"

      # ── Storage / cloud ────────────────────────────────────────────────────
      "daisydisk"
      "google-drive"
      "the-unarchiver"

      # ── Development tools ──────────────────────────────────────────────────
      "postman"
      "orbstack"
      "parallels"
      "warp"
      # "docker-desktop"

      # ── Utilities ──────────────────────────────────────────────────────────
      # Look into Tokie file manager in the future
      "purge" # cache and junk cleanup app (not macOS's /usr/sbin/purge)
      "rustdesk" # The free TeamViewer/AnyDesk solution
      "betterdisplay" # For creating virtual displays and stuf
    ]
    ++ personalOnly [
      # ── Personal accounts / leisure / home hardware ────────────────────────
      "notion"
      "notion-calendar"
      "chatgpt"
      "bitwarden"
      "tradingview"
      "whatsapp"
      "nextcloud"
      "paragon-extfs"
      "gimp"
      "iina"
      "spotify"
      "steam"
      "nvidia-geforce-now"
      "adguard"
      "hp-easy-start"
      "raspberry-pi-imager"
    ]
    ++ workOnly [
      # Nothing work-only yet.  VPN clients, MDM agents and the like go here.
    ];
  };
}
