# Nushell alongside Zsh

Status: built and tested, not activated (`make switch-locked` is the user's step)

## Why / scope
Try Nushell without giving up Zsh. Zsh stays the login shell and its
configuration is untouched; Nushell is installed next to it and started with
`nu`. The point of the experiment is that Nushell needs no plugins for the four
things Zsh currently loads plugins for: history hints, syntax highlighting,
fuzzy completion and a prompt.

Requested: reproduce the current setup, keep atuin and zoxide, carapace allowed,
drop the fzf integration.

## Behavior
- `nix/nushell.nix` holds the declarative half: `$env.config` settings, aliases,
  environment variables, and the integration switches.
- `configs/nushell/config.nu` holds the half that is genuinely code: the prompt,
  the commands replacing Zsh functions, and one keybinding. Loaded with
  `builtins.readFile`, matching how `configs/tmux.conf` is already handled.
- Integrations: atuin, zoxide and yazi gain a Nushell half; carapace is added and
  bridges to Zsh's completion definitions (`CARAPACE_BRIDGES = "zsh"`) for
  commands it has no spec for. fzf gains nothing, as requested.
- `completions.algorithm = "fuzzy"` is what makes carapace sufficient and
  `fzf-tab` unnecessary.

### What did not translate literally

| Zsh | Nushell | Why |
| --- | --- | --- |
| `pure-prompt` plugin | `PROMPT_COMMAND`/`PROMPT_INDICATOR` in config.nu | Same layout, same green/red and stash rules as the `zstyle`s |
| `ls`/`la`/`ll` via `gls` | Nushell's builtin `ls`, with `la`/`ll` as commands | Aliasing `ls` to an external would throw away the structured table that is the reason to use Nushell. `gls` is still on PATH |
| `lsg`/`lag`/`llg` (`*(/N)` globs) | `ls ... \| where type == dir` | Glob qualifiers are Zsh-only |
| `brewcl`/`brewall` (`&&`, `$(...)`) | commands using `;` and `(...)` | Neither operator exists in Nushell |
| `activate` (venv) | `def --env activate` + `deactivate` | CPython ships activate, .csh, .fish and .ps1 — no Nushell script |
| `opam env --shell=zsh` | parses `--shell=sh` output | `opam env` has no Nushell target |
| `zle`/`bindkey` widget | `$env.config.keybindings` entry | Same Option+Space, same script |
| `setopt hist_ignore_dups` | — | Nushell's SQLite history has no dedup switch; atuin is the search layer anyway |
| `INSIDE_EMACS_EAT` guard | dropped | Dead since the move to Ghostel |

History is 10M entries, SQLite, `isolation = false` to match Zsh's `share = true`.

## Evidence
- `nix flake check`: all checks passed (deadnix, statix, system build).
- `nix build .#darwinConfigurations.gauss.system`: succeeds. Formatting with the
  repo's `nixfmt-tree` did not change the output hash.
- The generated `config.nu` was extracted and run under real Nushell 0.115.1.
  Verified in that session: `edit_mode=emacs`, `completions.algorithm=fuzzy`,
  history `sqlite/10000000/isolation=false`, `EDITOR`/`VISUAL`/`_ZO_DOCTOR` set,
  carapace's external completer installed, both atuin keybindings present
  (`Ctrl-R` and `Up`), the cmux keybinding present, and the commands `cd`
  (zoxide), `y` (yazi), `la`, `ll`, `lsg`, `lag`, `llg`, `activate`,
  `deactivate`, `brewcl`, `brewall` all defined alongside the 14 aliases.
- Prompt rendered live: `~/Documents/GitHub/obelisk main*` in a dirty repo,
  `/tmp` outside one, `\n❯ ` as the indicator.
- `activate` was exercised against a throwaway venv: it sets `VIRTUAL_ENV`,
  prepends `bin` to PATH, and `deactivate` restores the original PATH.
- **Zsh is untouched, proven rather than asserted**: the generated `.zshrc` was
  evaluated from a detached worktree at HEAD and from the working tree, and the
  two are byte-identical. No carapace or Nushell lines leak into it.

### One upstream defect worked around

Atuin's own Nushell init gives both of its keybindings the name `atuin`, which
makes Nushell print a ~20-line `shared_keybindings_name` warning on **every**
startup — confirmed on a real tty, not just in script mode. Both bindings do
work, so it is cosmetic, but not at every prompt. `nix/nushell.nix` therefore
generates the same file home-manager would and renames the second binding with
`awk`. If upstream renames them, the pattern matches nothing and the workaround
becomes inert. Startup is now silent.

## Not done
- Not activated. `make switch-locked` needs the user's sudo password.
- `cmux` is still commented out in `nix/homebrew.nix`, so the Option+Space
  sessionizer binding is a no-op in Nushell exactly as it already is in Zsh.
  Reproduced faithfully rather than silently dropped.
