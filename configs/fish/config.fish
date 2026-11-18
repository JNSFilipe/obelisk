# Fish interactive configuration.
#
# Nix owns the declarative half of this setup (environment variables, PATH,
# aliases, and plugin wiring for pure, autopair, and done); see nix/fish.nix.
# What lives here is the interactive Fish code replicated from baum (CachyOS
# fish config) combined with Obelisk's macOS helpers, zoxide, and atuin.
#
# Zsh remains the login shell. Start this one with `fish`.

# ── Greeting ─────────────────────────────────────────────────────────────────
# Run fastfetch on interactive startup when available (from baum/CachyOS).

function fish_greeting
    if command -q fastfetch
        fastfetch
    end
end

# ── History & file helpers (from baum / CachyOS) ─────────────────────────────
# Functions needed for !! and !$ (https://github.com/oh-my-fish/plugin-bang-bang)

function __history_previous_command
    switch (commandline -t)
        case "!"
            commandline -t $history[1]
            commandline -f repaint
        case "*"
            commandline -i !
    end
end

function __history_previous_command_arguments
    switch (commandline -t)
        case "!"
            commandline -t ""
            commandline -f history-token-search-backward
        case "*"
            commandline -i '$'
    end
end

if test "$fish_key_bindings" = fish_vi_key_bindings
    bind -Minsert ! __history_previous_command
    bind -Minsert '$' __history_previous_command_arguments
else
    bind ! __history_previous_command
    bind '$' __history_previous_command_arguments
end

function history
    builtin history --show-time='%F %T ' $argv
end

function backup --argument filename
    cp $filename $filename.bak
end

function copy
    set -l count (count $argv)
    if test "$count" = 2; and test -d "$argv[1]"
        set -l from (string trim --right --chars=/ -- $argv[1])
        set -l to $argv[2]
        command cp -r $from $to
    else
        command cp $argv
    end
end

# ── Directory-only listing (Obelisk zsh `*(/N)` equivalents) ─────────────────

function lsg
    set -l dirs */
    if test (count $dirs) -gt 0
        gls --color=auto -d -- (string trim --right --chars=/ -- $dirs) $argv
    end
end

function lag
    set -l dirs */ .*/
    set -l filtered
    for d in (string trim --right --chars=/ -- $dirs)
        if test "$d" != "." -a "$d" != ".."
            set -a filtered $d
        end
    end
    if test (count $filtered) -gt 0
        gls --color=auto -d -A -- $filtered $argv
    end
end

function llg
    set -l dirs */ .*/
    set -l filtered
    for d in (string trim --right --chars=/ -- $dirs)
        if test "$d" != "." -a "$d" != ".."
            set -a filtered $d
        end
    end
    if test (count $filtered) -gt 0
        gls --color=auto -d -al -- $filtered $argv
    end
end

# ── Homebrew helpers ─────────────────────────────────────────────────────────

function brewcl
    brew cleanup -s; and rm -rf (brew --cache)
end

function brewall
    brew upgrade --greedy
    and brew upgrade --cask --greedy
    and brewcl
    and brew uninstall microsoft-auto-update
end

# ── Python virtualenvs ───────────────────────────────────────────────────────

function activate
    if set -q VIRTUAL_ENV; and functions -q deactivate
        deactivate
    end
    for dir in venv .venv env .env virtualenv
        if test -d "$dir" -a -f "$dir/bin/activate.fish"
            echo "Activating virtual environment in $dir"
            source "$dir/bin/activate.fish"
            return 0
        end
    end
    echo "No Python virtual environment found."
    return 1
end

# ── Opam ─────────────────────────────────────────────────────────────────────

if command -q opam; and opam switch show >/dev/null 2>&1
    opam env --shell=fish | source
end

# ── Yazi wrapper (`y`) ───────────────────────────────────────────────────────

function y
    set -l tmp (mktemp -t "yazi-cwd.XXXXXX")
    command yazi $argv --cwd-file="$tmp"
    if set -l cwd (command cat -- "$tmp"); and test -n "$cwd" -a "$cwd" != "$PWD"
        builtin cd -- "$cwd"
    end
    rm -f -- "$tmp"
end

# ── Keybindings ──────────────────────────────────────────────────────────────
# Option+Space opens the cmux sessionizer, matching zsh and nushell.

function __cmux_sessionizer
    "$HOME/.config/scripts/cmux_sesh.sh"
    commandline -f repaint
end
bind alt-space __cmux_sessionizer
bind \xc2\xa0 __cmux_sessionizer

# ── Integrations (zoxide, fzf, atuin) ────────────────────────────────────────

/opt/homebrew/bin/zoxide init fish --cmd cd | source
set -gx FZF_CTRL_R_COMMAND ""
/opt/homebrew/bin/fzf --fish | source
/opt/homebrew/bin/atuin init fish | source

if set -q TMUX; and set -q ATUIN_SESSION
    /opt/homebrew/bin/tmux setenv ATUIN_SESSION "$ATUIN_SESSION"
end
