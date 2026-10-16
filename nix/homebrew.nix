{ hostProfile, lib, ... }:

# ── nix-darwin homebrew integration ──────────────────────────────────────────
# Homebrew is the default package source: anything homebrew-core ships lives
# here, CLI or GUI.  nix/packages.nix is the escape hatch for the few things
# brew cannot provide.  nix-darwin/home-manager still own the system, the
# dotfiles, and every programs.* module (git, tmux, zsh, fzf, atuin, zoxide,
# kitty, ghostty, helix, lazygit, yazi) — those pull their own
# nix packages and must not be duplicated here.

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
    taps = [ "d12frosted/emacs-plus" ];

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
      # ── Terminal utilities ─────────────────────────────────────────────────
      "coreutils" # GNU core utils, g-prefixed (gls, gdate, …)
      "moreutils" # sponge, vipe, ts, …
      "less"
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

      # ── Version control ────────────────────────────────────────────────────
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

      # ── CI / containers / databases ────────────────────────────────────────
      # OrbStack provides the Docker engine, CLI, Compose, and buildx.
      "kubernetes-cli" # kubectl
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
      "gemini-cli"
      "openspec"
      "codex-acp" # ACP adapter for Codex, used by Emacs agent-shell

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

      # ── Terminals ──────────────────────────────────────────────────────────
      # "kitty"
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
      "orion"
      "helium-browser"
      "zen" # Firefox-based; its profile is the one being synced between Macs

      # ── AI coding assistants (CLIs distributed only as casks) ──────────────
      "claude-code"
      "codex"

      # ── Fonts ──────────────────────────────────────────────────────────────
      # TODO: The fonts should be managed on the nix side of things
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
      "warp"
      # "docker-desktop"

      # ── Utilities ──────────────────────────────────────────────────────────
      # Look into Tokie file manager in the future
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
