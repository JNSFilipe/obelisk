{ pkgs, ... }:
{
  # ── CLI packages from nixpkgs ───────────────────────────────────────────────
  # Nix GUI apps live in nix/darwin.nix; Homebrew casks live in homebrew.nix.

  home.packages = with pkgs; [

    # ── Terminal utilities ────────────────────────────────────────────────────
    coreutils # GNU core utils (gls, gdate, …)
    moreutils # sponge, vipe, ts, …
    less
    wget
    htop
    nmap
    jq
    fd
    ripgrep
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    doxx # open word documents in terminal
    xleak # open excel documents in terminal
    rtk # For saving tokes: Yields chewedup output for common terminal tools

    # ── Keyboard remapping ────────────────────────────────────────────────────
    kanata

    # ── Version control ───────────────────────────────────────────────────────
    gh

    # ── SSH helpers ───────────────────────────────────────────────────────────
    autossh
    sshpass

    # ── Serial / networking ───────────────────────────────────────────────────
    picocom
    portal # file transfer to other devices

    # ── Build tools ───────────────────────────────────────────────────────────
    gnumake
    cmake
    pkg-config
    act # run GitHub Actions locally

    # ── Syntax / parsing ──────────────────────────────────────────────────────
    tree-sitter
    python3Packages.pygments # for syntax-highlighted previews

    # ── Editors (terminal) ────────────────────────────────────────────────────
    neovim

    # ── CI / containers ───────────────────────────────────────────────────────
    # docker             # CLI only — docker-desktop cask provides the daemon
    podman # CLI only
    krunkit # for podman
    kubectl
    postgresql # Postgres tooling

    # ── AWS ------─────────────────────────────────────────────────────────────
    awscli2

    # ── Programming languages ─────────────────────────────────────────────────
    # Python (use `uv` for project venvs; system python for tooling)
    python313
    uv
    ty
    python3Packages.python-lsp-server
    ruff # includes built-in LSP (replaces ruff-lsp)
    black

    # Rust (via rustup so toolchain updates work; or pin via nixpkgs)
    rustup # rustup manages rustc / cargo / rust-analyzer

    # Go
    go
    wails
    gopls

    # Node / JS
    nodejs
    bun

    # Zig
    zig

    # C / C++
    gcc
    libgccjit
    llvm
    armadillo # linear algebra
    openblas
    boost
    python3Packages.pybind11
    raylib
    libusb1 # USB device access (needed for rpiboot, etc.)
    rpiboot # boot Raspberry Pi CM/Zero over USB

    # AI coding assistants
    claude-code
    codex
    gemini-cli
    openspec

    # LaTeX
    tectonic

  ];
}
