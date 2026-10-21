{
  config,
  flakeRoot,
  homeDirectory,
  lib,
  pkgs,
  userName,
  ...
}:

let
  link = path: config.lib.file.mkOutOfStoreSymlink "${flakeRoot}/configs/${path}";
in
{
  imports = [
    ./packages.nix
    ./emacs.nix
    ./nushell.nix
  ];

  # ── Identity ────────────────────────────────────────────────────────────────

  home.username = userName;
  home.homeDirectory = homeDirectory;
  home.stateVersion = "24.11";

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  # Homebrew man-db is on systemPath; do not install another manual viewer.
  programs.man.package = null;

  targets.darwin.linkApps.enable = false;

  # ── PATH (available to all shells, including non-interactive scripts) ────
  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
    "$HOME/.config/scripts"
    "/Library/TeX/texbin"
  ];

  # ── Config file symlinks ─────────────────────────────────────────────────────
  # Configs managed by programs.* modules are NOT listed here.
  # Only configs without a native home-manager module are symlinked.

  home.file = {
    # ── Editors ────────────────────────────────────────────────────────────
    ".config/doom".source = link "doom";
    ".config/nvim".source = link "nvim";
    ".config/vemacs".source = link "vemacs";
    ".config/gloom-emacs".source = link "gloom";
    ".config/zed".source = link "zed";

    # ── Terminals ──────────────────────────────────────────────────────────
    ".config/wezterm".source = link "wezterm";
    # ── Window management ──────────────────────────────────────────────────
    ".config/herdr/config.toml".source = link "herdr/config.toml";
    # File, not the directory: OmniWM keeps other state under ~/.config/omniwm.
    ".config/omniwm/settings.toml".source = link "omniwm/settings.toml";

    # ── Syncthing ──────────────────────────────────────────────────────────
    # One ignore list, linked into each shared folder.  The folders themselves
    # are paired through the Syncthing UI; only these two are shared, so no
    # database or browser profile is ever replicated.
    ".claude/projects/.stignore".source = link "syncthing/stignore";
    ".codex/sessions/.stignore".source = link "syncthing/stignore";

    # ── Misc tools ─────────────────────────────────────────────────────────
    ".config/scripts".source = link "scripts";

    # ── Desktop ────────────────────────────────────────────────────────────
    ".config/wallpapers".source = link "wallpapers";
  };

  # ── Syncthing ───────────────────────────────────────────────────────────────
  # Homebrew supplies the binary (nix/homebrew.nix); launchd owns the process,
  # the same arrangement as the Emacs daemon in nix/emacs.nix.  --no-restart
  # hands restarts to launchd rather than letting Syncthing respawn itself,
  # which would leave KeepAlive fighting a process it does not own.
  #
  # Devices are paired once through the web UI on http://127.0.0.1:8384; device
  # ids do not exist until first run, so they cannot be declared here.
  launchd.agents.syncthing = {
    enable = true;
    config = {
      ProgramArguments = [
        "/opt/homebrew/bin/syncthing"
        "serve"
        "--no-browser"
        "--no-restart"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      ThrottleInterval = 30;
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/syncthing.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/syncthing.error.log";
    };
  };

  # macOS keeps display scaling in com.apple.windowserver.displays.plist, keyed
  # by display UUID and rewritten by WindowServer, so there is no defaults key
  # for it and nix-darwin has no option.  The script drives displayplacer, which
  # uses the same API the Displays pane does, and derives "More Space" as the
  # largest scaled mode rather than hardcoding a resolution — so both machines
  # share it even with different built-in panels.  It exits 0 when displayplacer
  # or a GUI session is missing, so a headless activation cannot fail on it.
  # `|| true`: the script guards its own failure modes, but a display tweak is
  # never worth aborting an activation over, so any surprise stays a warning.
  home.activation.displayMoreSpace = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run ${flakeRoot}/configs/scripts/display-more-space || true
  '';

  # Raycast owned ⌘Space and disabled Spotlight's hotkey to get it.  With
  # Raycast removed, hand ⌘Space back to Spotlight: a disabled hotkey persists
  # in the defaults database, so deleting the Raycast block on its own would
  # leave the key doing nothing at all.  Delete this block if something else
  # should claim ⌘Space.
  home.activation.spotlightHotkey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run /usr/bin/defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys \
      -dict-add 64 \
      '<dict><key>enabled</key><true/><key>value</key><dict><key>parameters</key><array><integer>65535</integer><integer>49</integer><integer>1048576</integer></array><key>type</key><string>standard</string></dict></dict>'
    run /usr/bin/killall Spotlight || true
  '';

  # ── Zsh ─────────────────────────────────────────────────────────────────────

  programs.zsh = {
    enable = true;
    package = null; # Homebrew owns the shell; Home Manager only writes config.
    completionInit = ""; # zsh-autocomplete owns compinit.

    # History
    history = {
      size = 10000000;
      save = 10000000;
      path = "${config.home.homeDirectory}/.zsh_history";
      share = true;
      ignoreDups = true;
      ignoreAllDups = true;
    };

    initContent = lib.mkMerge [
      # Run before plugin loading (not just before the first prompt).
      # zsh-autocomplete sets recent-dirs-file here but never creates its parent.
      (lib.mkBefore ''
        mkdir -p -- "''${XDG_DATA_HOME:-$HOME/.local/share}/zsh"
        fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
      '')

      # Homebrew paths stay stable across upgrades. No build-time invocation
      # or Nix copy of these packages is needed to generate the dotfiles.
      (lib.mkOrder 580 ''
        source /opt/homebrew/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh
        source /opt/homebrew/opt/fzf-tab/share/fzf-tab/fzf-tab.zsh
        source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
        eval "$(/opt/homebrew/bin/zoxide init zsh --cmd cd)"
        if [[ $options[zle] = on ]]; then
          export FZF_CTRL_R_COMMAND=""
          source <(/opt/homebrew/bin/fzf --zsh)
          eval "$(/opt/homebrew/bin/atuin init zsh)"
        fi
      '')

      # Zsh options matching the original zshrc
      ''
        # ── Options ──────────────────────────────────────────────────────────
        setopt interactive_comments
        setopt appendhistory
        setopt hist_save_no_dups
        setopt hist_ignore_dups

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
        # Most commands live in configs/scripts/ (on PATH).
        # activate must be a function because it modifies the calling shell.

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

        if command -v opam >/dev/null 2>&1 && opam switch show >/dev/null 2>&1; then
          eval "$(opam env --shell=zsh)"
        fi

        # Option+Space arrives as Meta-Space or NBSP depending on terminal Option handling.
        cmux-sessionizer-widget() {
          zle -I
          "$HOME/.config/scripts/cmux_sesh.sh"
          zle reset-prompt
        }
        zle -N cmux-sessionizer-widget
        bindkey $'\e ' cmux-sessionizer-widget
        bindkey $'\xc2\xa0' cmux-sessionizer-widget
      ''
    ];

    shellAliases = {
      # ls variants — gls is GNU ls from brew coreutils; Apple's ls stays as
      # /bin/ls and does not understand --color=auto.
      ls = "gls --color=auto";
      la = "gls --color=auto -A";
      ll = "gls --color=auto -al";
      lsg = "gls --color=auto -d -- *(/N)";
      lag = "gls --color=auto -d -A -- *(/N) .*(/N)";
      llg = "gls --color=auto -d -al -- *(/N) .*(/N)";
      # Navigation: Homebrew zoxide's init above supplies cd.
      # Editors
      vi = "nvim";
      vim = "nvim";
      # Frames from the launchd-supervised daemon (no second daemon owner).
      ec = "emacsclient -c";
      et = "emacsclient -t";
      # Network
      ww = "wget";
      # System
      del = "sudo rm -r";
      comp = "sudo make install";
      # Git
      gl = "git clone";
      lg = "lazygit";
      rcp = "rsync -avzh --progress --stats";
      # Homebrew helpers (kept for CLI convenience during transition)
      brewdeps = "brew deps --installed --tree";
      brewup = "brew upgrade --greedy";
      caskup = "brew upgrade --cask --greedy";
      brewcl = "brew cleanup -s && rm -rf $(brew --cache)";
      brewrm-msupdate = "brew uninstall microsoft-auto-update";
      brewall = "brewup && caskup && brewcl && brewrm-msupdate";
    };
  };

  # ── Atuin (shell history) ────────────────────────────────────────────────────

  # The upstream module requires a Nix package, so render settings directly
  # and initialize the Homebrew binary in each shell instead.
  xdg.configFile."atuin/config.toml".source = (pkgs.formats.toml { }).generate "atuin-config" {
    search_mode = "daemon-fuzzy";
    filter_mode = "global";
    filter_mode_shell_up_key_binding = "global";
    search_mode_shell_up_key_binding = "fuzzy";
    style = "compact";
    ctrl_n_shortcuts = true;
    enter_accept = true;
    sync.records = true;
    daemon = {
      enabled = true;
      autostart = true;
    };
    ai.enabled = true;
  };

  home.sessionVariables = {
    _ZO_DOCTOR = "0"; # suppress false-positive init order warning
    # Talk to the Emacs daemon instead of starting a fresh Emacs per edit.
    # EDITOR stays in the terminal (git commit, crontab, …); VISUAL opens a GUI
    # frame, following the usual split. launchd restarts the daemon if it crashes.
    EDITOR = "emacsclient -t";
    VISUAL = "emacsclient -c";
  };

  # ── Tmux ─────────────────────────────────────────────────────────────────

  programs.tmux = {
    enable = true;
    package = null; # Plugins below have no Homebrew formula; tmux itself does.
    shell = "/opt/homebrew/bin/zsh";
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
    enable = true;
    package = null; # installed via homebrew cask
    enableZshIntegration = false;
    settings = {
      theme = "Carbonfox";
      background-opacity = 1.0;
      window-padding-x = "2,2";
      window-padding-y = "2,2";
      shell-integration-features = "no-cursor";
      cursor-style = "block";
      cursor-style-blink = false;
      macos-titlebar-style = "hidden";
      bold-is-bright = true;
      font-family = "Iosevka";
      font-size = 13;
      keybind = [
        "unconsumed:ctrl+ç=reload_config"
        "alt+space=text:\\x1b\\x20"
        "alt+d=esc:d"
      ];
      macos-option-as-alt = true;
    };
  };

  # ── Kitty ───────────────────────────────────────────────────────────────────

  programs.kitty = {
    enable = true;
    package = null; # installed via Homebrew cask
    font = {
      name = "Iosevka";
      size = 13;
    };
    settings = {
      bold_font = "Iosevka Bold";
      italic_font = "Iosevka Italic";
      bold_italic_font = "Iosevka Bold Italic";
      disable_ligatures = "never";
      text_composition_strategy = "legacy";
      cursor_shape = "block";
      cursor_blink_interval = 0;
      scrollback_lines = 20000;
      scrollback_fill_enlarged_window = "yes";
      wheel_scroll_min_lines = 2;
      detect_urls = "yes";
      copy_on_select = "clipboard";
      strip_trailing_spaces = "smart";
      focus_follows_mouse = "yes";
      hide_window_decorations = "titlebar-only";
      enable_audio_bell = "no";
      background_opacity = 1;
      background_blur = 0;
      shell = "/opt/homebrew/bin/zsh";
      editor = "em";
      clipboard_control = "write-clipboard write-primary read-clipboard-ask read-primary-ask";
      clipboard_max_size = 512;
      allow_hyperlinks = "yes";
      shell_integration = "enabled no-cursor";
      allow_cloning = "ask";
      term = "xterm-kitty";
      macos_option_as_alt = "yes";
    };
    keybindings = {
      "cmd+q" = "quit";
    };
    extraConfig = builtins.readFile ../configs/kitty/tokyo-night.conf;
  };

  # ── Helix ───────────────────────────────────────────────────────────────────

  programs.helix = {
    enable = true;
    package = null;
    settings = {
      theme = "carbonfox";
      editor = {
        default-yank-register = "+";
        line-number = "relative";
        mouse = true;
        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "block";
        };
        file-picker.hidden = false;
      };
      keys.normal = {
        w = "@miw";
        "C-q" = "wclose";
        space = {
          F = "no_op";
          E = "no_op";
          e = "no_op";
          P = "no_op";
          R = "no_op";
          C = "no_op";
          D = "no_op";
          Y = "no_op";
          y = "no_op";
          "?" = "no_op";
          "/" = "no_op";
          "A-c" = "no_op";
          s = ":vsplit";
          S = ":hsplit";
          i = "symbol_picker";
          I = "workspace_symbol_picker";
          p = "global_search";
          d = "workspace_diagnostics_picker";
          w = ":w";
          o = [
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
          k = ":bc";
          u = "undo";
          h = "select_all";
        };
      };
      keys.insert = {
        "C-q" = "wclose";
        "C-g" = "normal_mode";
        j = {
          j = "normal_mode";
        };
        "C-a" = "goto_line_start";
        "C-e" = "goto_line_end";
        "C-f" = "move_char_right";
        "C-b" = "move_char_left";
        "C-n" = "move_line_down";
        "C-p" = "move_line_up";
        "A-f" = "move_next_word_start";
        "A-b" = "move_prev_word_start";
        "A-v" = "page_up";
        "C-v" = "page_down";
        "A-<" = "goto_file_start";
        "A->" = "goto_file_end";
        "C-l" = "align_view_center";
        "2" = "hsplit";
        "3" = "vsplit";
        "0" = "wclose";
        "1" = "wonly";
        "C-space" = "select_mode";
        "S-C-f" = "extend_char_right";
        "S-C-b" = "extend_char_left";
        "S-C-n" = "extend_line_down";
        "S-C-p" = "extend_line_up";
        "S-A-f" = "extend_next_word_start";
        "S-A-b" = "extend_prev_word_start";
        "C-d" = "delete_char_forward";
        "C-h" = "delete_char_backward";
        "A-d" = "delete_word_forward";
        "C-w" = "delete_word_backward";
        "A-backspace" = "delete_word_backward";
        "C-k" = "kill_to_line_end";
        "C-y" = "paste_before";
        "C-u" = "kill_to_line_start";
        "C-o" = "open_below";
        "C-j" = "insert_newline";
        "C-/" = "undo";
        "C-_" = "undo";
        "C-s" = "search";
        "C-x" = {
          "C-f" = "file_explorer";
          "C-b" = "buffer_picker";
          "C-s" = ":w";
          "C-c" = ":q";
          k = ":bc";
          u = "undo";
          h = "select_all";
        };
      };
    };
  };

  # ── Lazygit ─────────────────────────────────────────────────────────────────

  programs.lazygit = {
    enable = true;
    package = null;
  };

  # ── Yazi (file manager) ─────────────────────────────────────────────────────

  programs.yazi = {
    enable = true;
    package = null;
    enableZshIntegration = true;
    shellWrapperName = "y";
  };

  # ── Git ──────────────────────────────────────────────────────────────────────

  programs.git = {
    enable = true;
    package = null;
    lfs = {
      enable = true;
      package = null;
    };
    signing.format = null; # silence stateVersion warning
    ignores = [
      "**/.claude/settings.local.json"
    ];
    settings = {
      user.name = "JNSFilipe";
      user.email = "jose.filipe@ieee.org";
      init.defaultBranch = "main";
      pull.rebase = false;
    };
  };
}
