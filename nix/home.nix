{ pkgs, config, flakeRoot, ... }:

let
  # mkOutOfStoreSymlink creates a symlink to a live path rather than copying
  # into the nix store.  Edits to the source files take effect immediately
  # without a rebuild.  Requires the flakeRoot variable passed via extraSpecialArgs.
  link = path: config.lib.file.mkOutOfStoreSymlink "${flakeRoot}/configs/${path}";
in
{
  imports = [ ./packages.nix ];

  # ── Identity ────────────────────────────────────────────────────────────────

  home.username      = "jfilipe";
  home.homeDirectory = "/Users/jfilipe";
  home.stateVersion  = "24.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  # ── Config file symlinks ─────────────────────────────────────────────────────
  # All configs are symlinked from the live repo so edits are reflected instantly.

  home.file = {
    # ── Shell ──────────────────────────────────────────────────────────────
    # .zshrc is managed by programs.zsh below; no entry needed here.

    # ── Tmux ───────────────────────────────────────────────────────────────
    ".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink
      "${flakeRoot}/configs/tmux.conf";

    # ── Editors ────────────────────────────────────────────────────────────
    ".config/doom".source       = link "doom";
    ".config/nvim".source       = link "nvim";
    ".config/helix".source      = link "helix";
    ".config/emacs".source      = link "emacs";
    ".config/vemacs".source     = link "vemacs";
    ".config/zed".source        = link "zed";

    # ── Terminals ──────────────────────────────────────────────────────────
    ".config/ghostty".source    = link "ghostty";
    ".config/kitty".source      = link "kitty";
    ".config/wezterm".source    = link "wezterm";

    # ── Window / key management ────────────────────────────────────────────
    ".config/karabiner".source  = link "karabiner";
    ".config/kanata".source     = link "kanata";

    # ── Misc tools ─────────────────────────────────────────────────────────
    ".config/lazygit".source    = link "lazygit";
    ".config/atuin".source      = link "atuin";
    # ── Scripts (keep executable via stow path; home-manager just links dir)
    ".config/scripts".source    = link "scripts";

    # ── Desktop ────────────────────────────────────────────────────────────
    ".config/wallpapers".source = link "wallpapers";

    # ── Git global ignore ──────────────────────────────────────────────────
    ".config/git/ignore".source = link "git/ignore";
  };

  # ── Zsh ─────────────────────────────────────────────────────────────────────

  programs.zsh = {
    enable = true;

    # History
    history = {
      size       = 10000000;
      save       = 10000000;
      path       = "${config.home.homeDirectory}/.zsh_history";
      share      = true;
      ignoreDups = true;
      ignoreAllDups = true;
    };

    # Zsh options matching the original zshrc
    initContent = ''
      # ── Options ──────────────────────────────────────────────────────────
      setopt interactive_comments
      setopt appendhistory
      setopt hist_save_no_dups
      setopt hist_ignore_dups

      # ── PATH ─────────────────────────────────────────────────────────────
      export PATH="$PATH:$HOME/.local/bin"
      export PATH="$PATH:$HOME/.cargo/bin"
      export PATH="$PATH:$HOME/.config/scripts"
      export PATH="$PATH:/usr/local/bin"
      export PATH="$PATH:/opt/homebrew/bin"
      export PATH="$PATH:/opt/homebrew/sbin"
      export PATH="$PATH:/System/Cryptexes/App/usr/bin"
      export PATH="$PATH:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
      export PATH="$PATH:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
      export PATH="$PATH:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"
      export PATH="$PATH:/Library/TeX/texbin"
      export PATH="$PATH:/Applications/microchip/xc8/v3.10/bin"

      # ── Plugin manager (znap) ─────────────────────────────────────────────
      # Znap is kept here for a gradual migration.  To go fully nix-native,
      # replace with home-manager's programs.zsh.plugins entries.
      [[ -r ~/.znap/znap/znap.zsh ]] || \
        git clone --depth 1 -- \
          https://github.com/marlonrichert/zsh-snap.git ~/.znap/znap
      source ~/.znap/znap/znap.zsh

      autoload -Uz compinit
      compinit

      # fzf-tab — fuzzy tab completion
      znap source Aloxaf/fzf-tab

      # Fish-like completions (disabled inside Emacs eat buffer)
      if [[ -z "$INSIDE_EMACS_EAT" ]]; then
        znap source marlonrichert/zsh-autocomplete
        znap source zsh-users/zsh-autosuggestions
      fi

      # Atuin (history search) — managed via programs.atuin; shell init injected below
      znap source atuinsh/atuin

      # Pure prompt
      fpath+=($HOME/.znap/sindresorhus/pure)
      znap source sindresorhus/pure
      autoload -U promptinit; promptinit
      zstyle :prompt:pure:prompt:success color green
      zstyle :prompt:pure:prompt:error   color red
      zstyle :prompt:pure:git:stash      show yes
      prompt pure

      # Propagate ATUIN_SESSION into tmux display-popups
      [[ -n "$TMUX" && -n "$ATUIN_SESSION" ]] && \
        tmux setenv ATUIN_SESSION "$ATUIN_SESSION"

      # ── OCaml (opam) ──────────────────────────────────────────────────────
      [[ ! -r ~/.opam/opam-init/init.zsh ]] || \
        source ~/.opam/opam-init/init.zsh > /dev/null 2>&1

      # ── Functions ─────────────────────────────────────────────────────────

      # yazi: change cwd on exit
      y() {
        local tmp cwd
        tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
        command yazi "$@" --cwd-file="$tmp"
        IFS= read -r -d "" cwd < "$tmp"
        [ "$cwd" != "$PWD" ] && [ -d "$cwd" ] && builtin cd -- "$cwd"
        rm -f -- "$tmp"
      }

      # activate: find and source a Python venv in the current directory
      activate() {
        if [ -n "$VIRTUAL_ENV" ]; then deactivate; fi
        local venv_dirs=("venv" ".venv" "env" ".env" "virtualenv")
        for dir in "''${venv_dirs[@]}"; do
          if [ -d "$dir" ] && [ -f "$dir/bin/activate" ]; then
            echo "Activating virtual environment in $dir"
            source "$dir/bin/activate"
            return 0
          fi
        done
        echo "No Python virtual environment found."
        return 1
      }

      # em: launch lightweight emacs (auto-installs python-lsp-server in venv)
      em() {
        if activate 2>/dev/null; then
          if ! uv pip show python-lsp-server &>/dev/null; then
            echo "python-lsp-server not found. Installing..."
            uv add "python-lsp-server[pyflakes]"
          fi
          emacs --init-directory ~/.config/emacs/ -nw "$@"
        else
          emacs --init-directory ~/.config/emacs/ -nw "$@"
        fi
      }

      # vem: launch vanilla emacs config in terminal
      vem() { emacs --init-directory ~/.config/vemacs/ -nw "$@"; }

      # essh: open GUI Emacs via TRAMP on a remote host
      essh() {
        if [ $# -eq 0 ]; then
          echo "Usage: essh [user@]hostname [-p port]"
          return 1
        fi
        local ssh_args=() host="" port=""
        while [[ $# -gt 0 ]]; do
          case $1 in
            -p) port="$2"; ssh_args+=("$1" "$2"); shift 2 ;;
            -*) ssh_args+=("$1"); shift ;;
            *)  [[ -z "$host" ]] && host="$1" || ssh_args+=("$1"); shift ;;
          esac
        done
        [[ -z "$host" ]] && { echo "Error: no hostname"; return 1; }
        local tramp_path="/ssh:$host:"
        if [[ -n "$port" ]]; then
          if [[ "$host" == *"@"* ]]; then
            tramp_path="/ssh:''${host%@*}@''${host#*@}#$port:"
          else
            tramp_path="/ssh:$host#$port:"
          fi
        fi
        echo "Opening Emacs TRAMP: $tramp_path"
        nohup emacs "$tramp_path" >/dev/null 2>&1 &
      }

      # tramp: terminal Emacs eshell on a remote host
      tramp() {
        [[ -z "$1" ]] && { echo "Usage: tramp user@host [path]"; return 1; }
        local userhost="$1" path="''${2:-~}"
        emacs -nw --eval "(let ((default-directory \"/ssh:''${userhost}:''${path}/\")) (eshell t))"
      }

      # pssh: persistent SSH with auto-reconnect (uses autossh + sshpass)
      pssh() {
        if [[ -z "$1" ]]; then
          echo "Usage: pssh user@host [ssh-args...]"
          return 1
        fi
        local userhost="$1"; shift
        local pw
        read -rs "pw?Password for ''${userhost}: "
        echo
        SSHPASS="$pw" AUTOSSH_SSH="sshpass -e ssh" \
        autossh -M 0 \
          -o ServerAliveInterval=30 \
          -o ServerAliveCountMax=3 \
          -o StrictHostKeyChecking=accept-new \
          "$userhost" "$@"
        unset pw
      }
    '';

    shellAliases = {
      # ls variants
      ls   = "ls --color=auto";
      la   = "ls --color=auto -A";
      ll   = "ls --color=auto -al";
      lsg  = "ls --color=auto -d -- *(/N)";
      lag  = "ls --color=auto -d -A -- *(/N) .*(/N)";
      llg  = "ls --color=auto -d -al -- *(/N) .*(/N)";
      # Navigation
      cd   = "z";
      # Editors
      vi   = "nvim";
      vim  = "nvim";
      # Network
      ww   = "wget";
      # System
      del  = "sudo rm -r";
      comp = "sudo make install";
      # Git
      gl   = "git clone";
      lg   = "lazygit";
      rcp  = "rsync -avzh --progress --stats";
      # Homebrew helpers (kept for CLI convenience during transition)
      brewdeps       = "brew deps --installed --tree";
      brewup         = "brew upgrade --greedy";
      caskup         = "brew upgrade --cask --greedy";
      brewcl         = "brew cleanup -s && rm -rf $(brew --cache)";
      brewrm-msupdate = "brew uninstall microsoft-auto-update";
      brewall        = "brewup && caskup && brewcl && brewrm-msupdate";
    };
  };

  # ── Atuin (shell history) ────────────────────────────────────────────────────

  programs.atuin = {
    enable       = true;
    enableZshIntegration = false; # znap loads atuin; avoid double-init
    # Config sourced from the live repo file (home.file.".config/atuin" above)
  };

  # ── Zoxide (smart cd) ────────────────────────────────────────────────────────

  programs.zoxide = {
    enable               = true;
    enableZshIntegration = true;
  };

  # ── Fzf ─────────────────────────────────────────────────────────────────────

  programs.fzf = {
    enable               = true;
    enableZshIntegration = false; # fzf-tab handles completions via znap
  };

  # ── Git ──────────────────────────────────────────────────────────────────────

  programs.git = {
    enable     = true;
    lfs.enable = true;
    signing.format = null; # silence stateVersion warning
    settings = {
      user.name       = "JNSFilipe";
      user.email      = "jose.filipe@ieee.org";
      init.defaultBranch = "main";
      pull.rebase     = false;
    };
  };
}
