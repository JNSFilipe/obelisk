{ pkgs, config, flakeRoot, ... }:

let
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
  # Configs managed by programs.* modules are NOT listed here.
  # Only configs without a native home-manager module are symlinked.

  home.file = {
    # ── Editors ────────────────────────────────────────────────────────────
    ".config/doom".source       = link "doom";
    ".config/nvim".source       = link "nvim";
    ".config/emacs".source      = link "emacs";
    ".config/vemacs".source     = link "vemacs";
    ".config/zed".source        = link "zed";

    # ── Terminals ──────────────────────────────────────────────────────────
    ".config/wezterm".source    = link "wezterm";

    # ── Window / key management ────────────────────────────────────────────
    ".config/karabiner".source  = link "karabiner";
    ".config/kanata".source     = link "kanata";

    # ── Misc tools ─────────────────────────────────────────────────────────
    ".config/scripts".source    = link "scripts";

    # ── Desktop ────────────────────────────────────────────────────────────
    ".config/wallpapers".source = link "wallpapers";
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

      # ── PATH (only paths not already provided by nix or macOS) ──────────
      export PATH="$PATH:$HOME/.local/bin"
      export PATH="$PATH:$HOME/.cargo/bin"
      export PATH="$PATH:$HOME/.config/scripts"
      export PATH="$PATH:/Library/TeX/texbin"

      # ── Completions ────────────────────────────────────────────────────────
      autoload -Uz compinit
      compinit

      # ── Disable fish-like completions inside Emacs eat ─────────────────
      if [[ -n "$INSIDE_EMACS_EAT" ]]; then
        ZSH_AUTOSUGGEST_DISABLE=1
        zstyle ':autocomplete:*' enabled no
      fi

      # ── Pure prompt ──────────────────────────────────────────────────────
      autoload -U promptinit; promptinit
      zstyle :prompt:pure:prompt:success color green
      zstyle :prompt:pure:prompt:error   color red
      zstyle :prompt:pure:git:stash      show yes
      prompt pure

      # Propagate ATUIN_SESSION into tmux display-popups
      [[ -n "$TMUX" && -n "$ATUIN_SESSION" ]] && \
        tmux setenv ATUIN_SESSION "$ATUIN_SESSION"

      # ── Functions ─────────────────────────────────────────────────────────

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

    plugins = [
      { name = "fzf-tab";            src = "${pkgs.zsh-fzf-tab}/share/fzf-tab"; }
      { name = "zsh-autocomplete";   src = "${pkgs.zsh-autocomplete}/share/zsh-autocomplete"; }
      { name = "zsh-autosuggestions"; src = "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions"; }
      { name = "pure";               src = "${pkgs.pure-prompt}/share/zsh/site-functions";
        file = "prompt_pure_setup"; }
    ];

    shellAliases = {
      # ls variants
      ls   = "ls --color=auto";
      la   = "ls --color=auto -A";
      ll   = "ls --color=auto -al";
      lsg  = "ls --color=auto -d -- *(/N)";
      lag  = "ls --color=auto -d -A -- *(/N) .*(/N)";
      llg  = "ls --color=auto -d -al -- *(/N) .*(/N)";
      # Navigation (zoxide provides 'z'; cd alias not needed with enableZshIntegration)
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
    enable               = true;
    enableZshIntegration = true;
    settings = {
      search_mode                      = "daemon-fuzzy";
      filter_mode                      = "global";
      filter_mode_shell_up_key_binding = "global";
      search_mode_shell_up_key_binding = "fuzzy";
      style                            = "compact";
      ctrl_n_shortcuts                 = true;
      enter_accept                     = true;
      sync.records                     = true;
      daemon = {
        enabled   = true;
        autostart = true;
      };
      ai.enabled = true;
    };
  };

  # ── Zoxide (smart cd) ────────────────────────────────────────────────────────

  programs.zoxide = {
    enable               = true;
    enableZshIntegration = true;
  };

  # ── Fzf ─────────────────────────────────────────────────────────────────────

  programs.fzf = {
    enable               = true;
    enableZshIntegration = true;
  };

  # ── Tmux ─────────────────────────────────────────────────────────────────

  programs.tmux = {
    enable  = true;
    plugins = with pkgs.tmuxPlugins; [
      yank
      {
        plugin = catppuccin;
        extraConfig = ''
          # Theme Stuff
          set -g @catppuccin_window_status_style "rounded"
          run #{plugin}/share/tmux-plugins/catppuccin/catppuccin.tmux
          set -g status-right-length 100
          set -g status-left-length 100
          set -g status-left ""
          set -g status-left "#{E:@catppuccin_status_session}"
          set -g status-right "#{E:@catppuccin_status_application}"
          # --> Oxocarbon
          set -g @thm_bg "#161616"
          set -g @thm_fg "#dde1e6"
          set -g @thm_rosewater "#ff7eb6"
          set -g @thm_flamingo "#ffab91"
          set -g @thm_pink "#ff7eb6"
          set -g @thm_mauve "#be95ff"
          set -g @thm_red "#ee5396"
          set -g @thm_maroon "#08bdba"
          set -g @thm_peach "#ff6f00"
          set -g @thm_yellow "#82cfff"
          set -g @thm_green "#42be65"
          set -g @thm_teal "#3ddbd9"
          set -g @thm_sky "#78a9ff"
          set -g @thm_sapphire "#82cfff"
          set -g @thm_blue "#33b1ff"
          set -g @thm_lavender "#be95ff"
          set -g @thm_subtext_1 "#f2f4f8"
          set -g @thm_subtext_0 "#dde1e6"
          set -g @thm_overlay_2 "#262626"
          set -g @thm_overlay_1 "#393939"
          set -g @thm_overlay_0 "#525252"
          set -g @thm_surface_2 "#525252"
          set -g @thm_surface_1 "#393939"
          set -g @thm_surface_0 "#262626"
          set -g @thm_mantle "#161616"
          set -g @thm_crust "#121212"
        '';
      }
    ];
    extraConfig = builtins.readFile ../configs/tmux.conf;
  };

  # ── Ghostty ──────────────────────────────────────────────────────────────────

  programs.ghostty = {
    enable          = true;
    package         = null; # installed via homebrew cask
    enableZshIntegration = true;
    settings = {
      theme                    = "Carbonfox";
      background-opacity       = 1.0;
      macos-option-as-alt      = "left";
      window-padding-x         = "2,2";
      window-padding-y         = "2,2";
      shell-integration-features = "no-cursor";
      cursor-style             = "block";
      cursor-style-blink       = false;
      macos-titlebar-style     = "hidden";
      bold-is-bright           = true;
      font-family              = "Iosevka";
      font-size                = 13;
      keybind                  = "unconsumed:ctrl+ç=reload_config";
    };
  };

  # ── Kitty ───────────────────────────────────────────────────────────────────

  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka";
      size = 13;
    };
    settings = {
      bold_font                        = "Iosevka Bold";
      italic_font                      = "Iosevka Italic";
      bold_italic_font                 = "Iosevka Bold Italic";
      disable_ligatures                = "never";
      text_composition_strategy        = "legacy";
      cursor_shape                     = "block";
      cursor_blink_interval            = 0;
      scrollback_lines                 = 20000;
      scrollback_fill_enlarged_window  = "yes";
      wheel_scroll_min_lines           = 2;
      detect_urls                      = "yes";
      copy_on_select                   = "clipboard";
      strip_trailing_spaces            = "smart";
      focus_follows_mouse              = "yes";
      hide_window_decorations          = "titlebar-only";
      enable_audio_bell                = "no";
      background_opacity               = 1;
      background_blur                  = 0;
      shell                            = "zsh";
      editor                           = "em";
      clipboard_control                = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
      clipboard_max_size               = 512;
      allow_hyperlinks                 = "yes";
      shell_integration                = "enabled no-cursor";
      allow_cloning                    = "ask";
      term                             = "xterm-kitty";
      macos_option_as_alt              = "yes";
    };
    keybindings = {
      "cmd+q" = "quit";
    };
    extraConfig = builtins.readFile ../configs/kitty/tokyo-night.conf;
  };

  # ── Helix ───────────────────────────────────────────────────────────────────

  programs.helix = {
    enable = true;
    settings = {
      theme = "carbonfox";
      editor = {
        default-yank-register = "+";
        line-number           = "relative";
        mouse                 = true;
        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "block";
        };
        file-picker.hidden = false;
      };
      keys.normal = {
        w     = "@miw";
        "C-q" = "wclose";
        space = {
          F       = "no_op";
          E       = "no_op";
          e       = "no_op";
          P       = "no_op";
          R       = "no_op";
          C       = "no_op";
          D       = "no_op";
          Y       = "no_op";
          y       = "no_op";
          "?"     = "no_op";
          "/"     = "no_op";
          "A-c"   = "no_op";
          s       = ":vsplit";
          S       = ":hsplit";
          i       = "symbol_picker";
          I       = "workspace_symbol_picker";
          p       = "global_search";
          d       = "workspace_diagnostics_picker";
          w       = ":w";
          o       = [
            ":sh rm -f /tmp/unique-file"
            ":insert-output yazi \"%{buffer_name}\" --chooser-file=/tmp/unique-file"
            ":sh printf \"\\x1b[?1049h\\x1b[?2004h\" > /dev/tty"
            ":open %sh{cat /tmp/unique-file}"
            ":redraw"
          ];
          "space" = "command_palette";
        };
        "C-x" = {
          "C-f" = "file_explorer";
          "C-b" = "buffer_picker";
          "C-s" = ":w";
          "C-c" = ":q";
          k     = ":bc";
          u     = "undo";
          h     = "select_all";
        };
      };
      keys.insert = {
        "C-q"   = "wclose";
        "C-g"   = "normal_mode";
        j       = { j = "normal_mode"; };
        "C-a"   = "goto_line_start";
        "C-e"   = "goto_line_end";
        "C-f"   = "move_char_right";
        "C-b"   = "move_char_left";
        "C-n"   = "move_line_down";
        "C-p"   = "move_line_up";
        "A-f"   = "move_next_word_start";
        "A-b"   = "move_prev_word_start";
        "A-v"   = "page_up";
        "C-v"   = "page_down";
        "A-<"   = "goto_file_start";
        "A->"   = "goto_file_end";
        "C-l"   = "align_view_center";
        "2"     = "hsplit";
        "3"     = "vsplit";
        "0"     = "wclose";
        "1"     = "wonly";
        "C-space" = "select_mode";
        "S-C-f" = "extend_char_right";
        "S-C-b" = "extend_char_left";
        "S-C-n" = "extend_line_down";
        "S-C-p" = "extend_line_up";
        "S-A-f" = "extend_next_word_start";
        "S-A-b" = "extend_prev_word_start";
        "C-d"   = "delete_char_forward";
        "C-h"   = "delete_char_backward";
        "A-d"   = "delete_word_forward";
        "C-w"   = "delete_word_backward";
        "A-backspace" = "delete_word_backward";
        "C-k"   = "kill_to_line_end";
        "C-y"   = "paste_before";
        "C-u"   = "kill_to_line_start";
        "C-o"   = "open_below";
        "C-j"   = "insert_newline";
        "C-/"   = "undo";
        "C-_"   = "undo";
        "C-s"   = "search";
        "C-x" = {
          "C-f" = "file_explorer";
          "C-b" = "buffer_picker";
          "C-s" = ":w";
          "C-c" = ":q";
          k     = ":bc";
          u     = "undo";
          h     = "select_all";
        };
      };
    };
  };

  # ── Lazygit ─────────────────────────────────────────────────────────────────

  programs.lazygit = {
    enable = true;
  };

  # ── Yazi (file manager) ─────────────────────────────────────────────────────

  programs.yazi = {
    enable               = true;
    enableZshIntegration = true;
    shellWrapperName     = "y";
  };

  # ── Opam (OCaml) ────────────────────────────────────────────────────────────

  programs.opam = {
    enable               = true;
    enableZshIntegration = true;
  };

  # ── Git ──────────────────────────────────────────────────────────────────────

  programs.git = {
    enable     = true;
    lfs.enable = true;
    signing.format = null; # silence stateVersion warning
    ignores = [
      "**/.claude/settings.local.json"
    ];
    settings = {
      user.name       = "JNSFilipe";
      user.email      = "jose.filipe@ieee.org";
      init.defaultBranch = "main";
      pull.rebase     = false;
    };
  };
}
