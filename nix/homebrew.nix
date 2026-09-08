_: {
  # ── nix-darwin homebrew integration ────────────────────────────────────────
  # GUI apps only.  All CLI tools live in nix/packages.nix.
  # Exceptions: brews[] holds CLI tools not yet packaged in nixpkgs;
  #             some casks, such as claude, are CLIs
  #             distributed only as macOS casks — no nix alternative.

  homebrew = {
    enable = true;

    onActivation = {
      # Keep system activation repeatable and non-destructive. Updates happen
      # explicitly through `make brew-upgrade`; drift aborts instead of deleting.
      autoUpdate = false;
      upgrade = false;
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
      "herdr"
      "mqttui"

      # ── Networking / tunnels ───────────────────────────────────────────────
      "cloudflared" # Cloudflare Tunnel client
      # nix-darwin's services.tailscale still runs the daemon; this declares the
      # brew copy so it stops showing up as drift. Nix paths come first on PATH,
      # so the nix `tailscale` CLI is the one that answers.
      "tailscale"

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

      # ── LaTeX ──────────────────────────────────────────────────────────────
      "tectonic"
    ];

    # ── Casks (GUI applications, plus CLIs shipped only as casks) ────────────

    casks = [
      # ── Terminals ──────────────────────────────────────────────────────────
      # "kitty"
      # "cmux"
      # "wezterm"
      "ghostty"

      # ── Editors ────────────────────────────────────────────────────────────
      "zed"
      "datagrip"
      "antigravity-cli"
      "sublime-text"
      # "visual-studio-code"

      # ── Version control ────────────────────────────────────────────────────
      "github" # GitHub Desktop

      # ── Window / keyboard management ──────────────────────────────────────
      # "kindavim"
      # "dockdoor"
      "hammerspoon"

      # ── Browsers ───────────────────────────────────────────────────────────
      "firefox"
      "helium-browser"

      # AI coding assistants moved to packages.nix (nix-managed)

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
      "notion"
      "claude"
      "chatgpt"
      "bitwarden"
      "codex-app"
      "notion-calendar"
      "raycast"
      "shortcat"
      "tradingview"

      # ── Communication ──────────────────────────────────────────────────────
      "zoom"
      "whatsapp"
      "microsoft-teams"

      # ── Microsoft Office ───────────────────────────────────────────────────
      "microsoft-word"
      "microsoft-excel"
      "microsoft-powerpoint"

      # ── Storage / cloud ────────────────────────────────────────────────────
      "daisydisk"
      "google-drive"
      "nextcloud"
      "paragon-extfs"
      "the-unarchiver"

      # ── Development tools ──────────────────────────────────────────────────
      "postman"
      "orbstack"
      "warp"
      # "docker-desktop"

      # ── Media / creative ───────────────────────────────────────────────────
      "gimp"
      "iina"
      "spotify"

      # ── Gaming ─────────────────────────────────────────────────────────────
      "steam"
      "nvidia-geforce-now"

      # ── Utilities ──────────────────────────────────────────────────────────
      # Look into Tokie file manager in the future
      "adguard"
      "rustdesk" # The free TeamViewer/AnyDesk solution
      "betterdisplay" # For creating virtual displays and stuf
      "hp-easy-start"
      "raspberry-pi-imager"
    ];
  };
}
