# Nushell interactive configuration.
#
# Nix owns the declarative half of this setup (aliases, $env.config settings,
# and the atuin/zoxide/yazi/carapace integrations); see nix/nushell.nix.  What
# lives here is the part that is genuinely code: the prompt, the commands that
# replace zsh functions, and one keybinding.
#
# Zsh remains the login shell.  Start this one with `nu`.

# ── Prompt ───────────────────────────────────────────────────────────────────
# A Pure-style prompt: path and git state on one line, the prompt character on
# the next, green normally and red after a failing command.  Matches the zsh
# `prompt pure` zstyles, stash indicator included.

def --wrapped _pure_run [...args: string]: nothing -> string {
    let result = (do --ignore-errors { ^git ...$args } | complete)
    if $result.exit_code != 0 { return "" }
    $result.stdout | str trim
}

def _pure_git []: nothing -> string {
    if (_pure_run rev-parse --is-inside-work-tree) != "true" { return "" }

    let branch = (_pure_run symbolic-ref --short HEAD)
    let name = if ($branch | is-empty) {
        let short = (_pure_run rev-parse --short HEAD)
        if ($short | is-empty) { "no-commits" } else { $short }
    } else {
        $branch
    }

    # A dirty tree gets Pure's asterisk; a non-empty stash gets its equals sign.
    let dirty = if (_pure_run status --porcelain | is-empty) { "" } else { "*" }
    let stash = if (_pure_run stash list | is-empty) { "" } else { "=" }

    $"(ansi grey)($name)($dirty)($stash)(ansi reset)"
}

def _pure_path []: nothing -> string {
    # Neither side is `path expand`ed: resolving symlinks would turn /tmp into
    # /private/tmp on macOS, which is not what the zsh prompt showed.
    let home = $env.HOME
    let cwd = $env.PWD
    let shown = if $cwd == $home {
        "~"
    } else if ($cwd | str starts-with $"($home)/") {
        $"~($cwd | str substring ($home | str length)..)"
    } else {
        $cwd
    }
    $"(ansi blue)($shown)(ansi reset)"
}

$env.PROMPT_COMMAND = {||
    let git = (_pure_git)
    if ($git | is-empty) { _pure_path } else { $"(_pure_path) ($git)" }
}

# Pure puts nothing on the right; keep the line clean.
$env.PROMPT_COMMAND_RIGHT = {|| "" }

$env.PROMPT_INDICATOR = {||
    let colour = if $env.LAST_EXIT_CODE == 0 { ansi green } else { ansi red }
    $"\n($colour)❯(ansi reset) "
}
$env.PROMPT_INDICATOR_VI_INSERT = $env.PROMPT_INDICATOR
$env.PROMPT_INDICATOR_VI_NORMAL = $env.PROMPT_INDICATOR
$env.PROMPT_MULTILINE_INDICATOR = {|| $"(ansi grey)::: (ansi reset)" }

# ── Listing ──────────────────────────────────────────────────────────────────
# Nushell's own `ls` returns a table, which is the entire point of the shell, so
# it is left alone rather than aliased to `gls`.  These are the zsh `la`/`ll`
# and the `*(/N)` glob-qualifier aliases, expressed as queries instead.
# `gls` is still on PATH when GNU output is actually wanted.

def la []: nothing -> table { ls --all }
def ll []: nothing -> table { ls --all --long }
def lsg []: nothing -> table { ls | where type == dir }
def lag []: nothing -> table { ls --all | where type == dir }
def llg []: nothing -> table { ls --all --long | where type == dir }

# ── Homebrew helpers ─────────────────────────────────────────────────────────
# `&&` and `$(...)` are not Nushell, so the compound zsh aliases become commands.

def brewcl [] {
    ^brew cleanup -s
    let cache = (^brew --cache | str trim)
    if ($cache | is-not-empty) and ($cache | path exists) {
        rm --recursive --force $cache
    }
}

def brewall [] {
    ^brew upgrade --greedy
    ^brew upgrade --cask --greedy
    brewcl
    ^brew uninstall microsoft-auto-update
}

# ── Python virtualenvs ───────────────────────────────────────────────────────
# CPython ships activate, activate.csh, activate.fish and Activate.ps1 — but no
# Nushell script — so the zsh `activate` function is reimplemented directly.
# `--env` is what lets it change the caller's environment.

def --env deactivate [] {
    if ("VIRTUAL_ENV" not-in $env) {
        print "No virtual environment is active."
        return
    }
    $env.PATH = ($env._OLD_VIRTUAL_PATH? | default $env.PATH)
    hide-env --ignore-errors VIRTUAL_ENV
    hide-env --ignore-errors _OLD_VIRTUAL_PATH
}

def --env activate [] {
    if ("VIRTUAL_ENV" in $env) { deactivate }

    let found = (
        ["venv" ".venv" "env" ".env" "virtualenv"]
        | where {|dir| ($dir | path join "bin" "activate" | path exists) }
        | first
    )

    if ($found | is-empty) {
        print "No Python virtual environment found."
        return
    }

    let root = ($found | path expand)
    print $"Activating virtual environment in ($found)"
    $env._OLD_VIRTUAL_PATH = $env.PATH
    $env.VIRTUAL_ENV = $root
    $env.PATH = ($env.PATH | prepend ($root | path join "bin"))
}

# ── Opam ─────────────────────────────────────────────────────────────────────
# `opam env` has no Nushell target, so the POSIX form is parsed instead.

def --env opam-env [] {
    if (which opam | is-empty) { return }
    let switch = (do --ignore-errors { ^opam switch show } | complete)
    if $switch.exit_code != 0 { return }

    ^opam env --shell=sh
    | lines
    | parse --regex '^(?<name>[A-Za-z_][A-Za-z0-9_]*)=(?<value>.*?); export'
    | each {|row| {name: $row.name, value: ($row.value | str trim --char "'")} }
    | reduce --fold {} {|row, acc| $acc | upsert $row.name $row.value }
    | load-env
}
opam-env

# ── Atuin inside tmux ────────────────────────────────────────────────────────
# Mirrors the zsh line that hands the session id to tmux display-popups.

if ("TMUX" in $env) and ("ATUIN_SESSION" in $env) {
    do --ignore-errors { ^tmux setenv ATUIN_SESSION $env.ATUIN_SESSION }
}

# ── Keybindings ──────────────────────────────────────────────────────────────
# Option+Space opens the cmux sessionizer, as in zsh.  Ghostty is configured
# with macos_option_as_alt = yes, so Option arrives as Alt here.

$env.config.keybindings = ($env.config.keybindings | append {
    name: cmux_sessionizer
    modifier: alt
    keycode: char_space
    mode: [emacs vi_insert vi_normal]
    event: {
        send: executehostcommand
        cmd: "^$\"($env.HOME)/.config/scripts/cmux_sesh.sh\""
    }
})
