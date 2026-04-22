{ pkgs, ... }:
{
  # ── CLI packages from nixpkgs ───────────────────────────────────────────────
  # GUI apps and homebrew-only formulae live in nix/homebrew.nix.

  home.packages = with pkgs; [

    # ── Terminal utilities ────────────────────────────────────────────────────
    coreutils          # GNU core utils (gls, gdate, …)
    moreutils          # sponge, vipe, ts, …
    less
    wget
    htop
    nmap
    jq
    fd
    ripgrep
    fzf
    aspell
    aspellDicts.en
    aspellDicts.en-computers

    # ── Shell history / navigation ────────────────────────────────────────────
    atuin
    zoxide

    # ── File managers ─────────────────────────────────────────────────────────
    yazi

    # ── Multiplexer ───────────────────────────────────────────────────────────
    tmux

    # ── Version control ───────────────────────────────────────────────────────
    git-lfs
    lazygit
    gh

    # ── SSH helpers ───────────────────────────────────────────────────────────
    autossh
    sshpass

    # ── Serial / networking ───────────────────────────────────────────────────
    picocom
    portal             # file transfer to other devices

    # ── Build tools ───────────────────────────────────────────────────────────
    gnumake
    cmake
    pkg-config
    act                # run GitHub Actions locally

    # ── Syntax / parsing ──────────────────────────────────────────────────────
    tree-sitter
    python3Packages.pygments   # for syntax-highlighted previews

    # ── Editors (terminal) ────────────────────────────────────────────────────
    neovim
    helix

    # ── CI / containers ───────────────────────────────────────────────────────
    docker             # CLI only — docker-desktop cask provides the daemon

    # ── Tailscale ─────────────────────────────────────────────────────────────
    tailscale

    # ── Programming languages ─────────────────────────────────────────────────
    # Python (use `uv` for project venvs; system python for tooling)
    python313
    uv
    python3Packages.python-lsp-server
    ruff               # includes built-in LSP (replaces ruff-lsp)
    black

    # Rust (via rustup so toolchain updates work; or pin via nixpkgs)
    rustup             # rustup manages rustc / cargo / rust-analyzer

    # Go
    go
    gopls

    # Node / JS
    nodejs
    bun

    # Zig
    zig

    # C / C++
    gcc
    llvm
    armadillo          # linear algebra
    openblas
    boost
    python3Packages.pybind11
    raylib

    # OCaml
    opam

  ];
}
