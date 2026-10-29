# Gloom Emacs

A parallel Emacs 30+ config written in plain Elisp. It shares the Doom config's
Sheprd implementation and Oxocarbon theme. It loads package libraries from the
existing Doom store, without starting Doom; editing uses Evil, as Doom does. `persp-mode` supplies sessions;
this deliberately uses the same package as Doom, rather than `perspective`.

## Run

From the repository, using the actual Emacs executable (the `emacs` shell wrapper
selects Doom):

```sh
/opt/homebrew/bin/emacs -Q --batch -l configs/gloom/install.el
/opt/homebrew/bin/emacs --init-directory="$PWD/configs/gloom"
```

The setup command installs only missing packages; on the current installation
Doom's store already provides every one. Setup also checks Ghostel's native
module. Normal startup never refreshes archives, downloads packages or installs
grammars. Ghostel (with evil-ghostel) and Doom Snippets are fetched from their
Git repositories using Emacs's built-in package-vc; the rest from the archives.
Native compilation uses Gloom's own `eln-cache` state directory. The reused
Persp/Yas/Doom Snippets libraries load with JIT compilation disabled for that
dependency path because their source builds contain undeclared compiler calls.
Other libraries retain native compilation and its normal warning reporting.

Home Manager declares `~/.config/gloom-emacs` and the `gloom` launcher (also
available as `gloom-emacs`). They
become available on the next normal system activation. The direct command above
works immediately. An optional separate daemon uses `gloom-emacs --daemon=gloom`;
connect with `/opt/homebrew/bin/emacsclient -s gloom -c` (or `-t`).

Runtime state, packages, backups, history and workspace saves go to
`~/.local/share/gloom-emacs`. `GLOOM_EMACS_STATE` overrides this directory.
Existing package builds are discovered under
`~/.local/share/doom-emacs/.local/straight/build-MAJOR.MINOR`; `DOOMLOCALDIR`
can select another store. The new config's own ELPA packages take precedence.
Do not delete the shared Doom package store while using these libraries. After
an Emacs version upgrade, sync Doom's builds or run this config's installer again.

## Editing and navigation

Editing is [Evil](https://github.com/emacs-evil/evil): Vim's states, motions,
operators (`dw`, `ci(`, `>>`...), text objects, registers, macros, `.` repeat
and ex commands. Special modes (Dired, Magit, Help, Info...) get Vim keys from
evil-collection, `ys`/`cs`/`ds` come from evil-surround and `gc` comments
with evil-nerd-commenter. Undo uses Emacs's own `undo-redo` (`u` / `C-r`).
Yanks go to the kill ring and the system clipboard.

The Doom config's Evil tweaks are kept: `j`/`k` move by visual line, `$` stops
at the last non-blank character, `w`/`W` select the word/WORD at point
(`viw`/`viW`), `<`/`>` keep the selection, `C-q` kills the buffer (also while
typing), and `ç`/`Ç` jump between Git hunks.

The leader is the `C-c` prefix map: `SPC` in normal, visual and motion states,
`M-SPC` in every state, so `SPC t`, `M-SPC t` and `C-c t` open the same
terminal. `SPC ?` lists the leader keys. The leader and normal-state
`C-h/j/k/l` (focus adjacent windows, including Ghostel terminals and the
protected Sheprd sidebar) take precedence over every mode's own bindings.
`C-h` still opens help in insert state. The sidebar binds these keys directly,
so you can navigate back to the editor or terminal. The cursor does not blink,
including Ghostel terminal requests for blinking.

| Key after SPC (or C-c) | Action |
| --- | --- |
| SPC | Command palette |
| t / , / < | New terminal / split right / split down |
| s / TAB | Sheprd sessions, panes and agents |
| f / p / b | Project file / project session / current session's buffers |
| o | Dired at the current file/directory |
| v / g / G | Search buffer / ripgrep project / Magit |
| w / c / u / y | Save / comment / visual undo / kill ring |
| m / M / r | Make target / compilation / shell command |
| D / e | Buffer diagnostics / code command prefix |
| - s / - h | Spell checking / fold at point |

`C-x b` also stays within the current session. Project switching creates/reuses
a session named by the full project root. Session layouts save on exit; use
`M-x persp-load-state-from-file` to restore the `workspaces/autosave` file.
Live shell/agent processes are not resurrected by saving a layout.

Sheprd's existing `C-c s` map includes `n/p` next/previous session, `N` new,
`W` rename, `D` kill, `1..9` select session, `v/-` splits, `h/j/k/l` focus,
`a` new agent (Claude, Codex or Pi, persistent and resumable; Pi can
run in Pilish), `!` next agent needing attention, `O` board, and `?` help. See
[Sheprd](../doom/sheprd.md#persistent-agents). Its sidebar requires graphical Emacs;
session and terminal commands also work in terminal frames. Terminals use
Ghostel's own Evil integration, evil-ghostel: they start in insert state with
the native shell keys (plus `C-s` to search the scrollback and `C-k` to kill to
the kill ring). **Escape** enters normal state, with output still streaming:
motions roam the scrollback, `v`/`y` copy, `C-h/j/k/l` change panes, and
operators (`d`, `c`, `x`...) edit the shell's input line. **i**/**a** return to
the live prompt. In full-screen programs such as Claude Code, Escape goes to the
program; `C-c <escape>` enters normal state there, and `C-c C-r` toggles that
per buffer. The sidebar and board keep their native buttons and `j/k`
movement. Sheprd's `e` opens editable scrollback in an ordinary buffer. Pilish
uses its own Evil integration. Dired uses `h/l` up/open, `j/k` movement, and its
built-in marks, operations, `wdired` and remote support.

## Coding and remote files

Eglot, Flymake, Xref, Eldoc and Emacs's completion preview provide code navigation,
diagnostics and completion. `M-/` opens completion through Consult/Vertico.
`SPC e a/r/f/s` invoke actions, rename, format and workspace symbols;
`SPC e d/R/h/D` invoke definition, references, documentation and project diagnostics.
Snippets reuse Yasnippet and the existing Doom snippet collection. Magit, diff-hl,
Avy, Vundo, Helpful, Consult GitHub and Makefile Executor are reused as well.

Flycheck handles local code buffers without Eglot; Eglot uses Flymake. `SPC e v`
evaluates Elisp with Eros overlays or runs other code with Quickrun. `SPC e j/T`
opens a language REPL/test runner; Python uses Pytest, Rust uses Rustic's Cargo
commands, and Go uses compilation. `SPC e c/B` checks/builds Cargo projects,
`SPC e E` expands Elisp macros and `SPC e i` selects snippets with Consult.
Emmet is active in web/CSS modes; `SPC e m` expands abbreviations. Python respects
explicit Pyvenv file/directory settings and offers Pipenv commands; local Node
projects expose `node_modules/.bin` before Eglot discovery. `SPC - d` opens Docker,
`SPC - r` repeats completion, and `SPC - y/Y` creates/expands an automatic snippet.
PDF Tools, PDF position restoration, Org PDF links, TOC, clipboard links, GitHub
Markdown export and lazy Babel language loading reuse the installed libraries.
The current Doom store has no built `epdfinfo` executable: PDF rendering needs
`M-x pdf-tools-install` and its native build dependencies. Missing PDF support
does not break Org, and startup never launches a native build.

See [the parity audit](PARITY.md) for what is equivalent, what changes with Gloom,
and the remaining differences from Doom. This is not an exhaustive reproduction
of Doom's module UI, templates or language command wrappers.

The language modes cover C/C++, Python, Go, Rust, JavaScript/TypeScript, web,
Lua, Nix, shell, JSON/YAML, Zig, Terraform, Docker, CSV, CMake, Markdown, Elisp
and Org. Installed native tree-sitter grammars select corresponding built-in
modes; existing non-tree-sitter modes provide the fallback. No new parser manager
or completion popup package is needed.

Language servers start automatically only when their executable exists (Python
uses the already-declared `pylsp`). `SPC e l` starts Eglot manually, including
its normal server selection prompt. Remote buffers check the remote executable
path. External servers, ripgrep, Git and agent CLIs must be installed where the
command runs; this config doesn't install system tools.

TRAMP keeps its built-in protocol support: SSH/SCP, sudo, multi-hop and available
container backends. Examples: `/ssh:host:/path`,
`/ssh:jump|ssh:target:/path`, `/ssh:host|sudo:root@host:/etc/hosts`, and
`/docker:container:/path`. Completion keeps basic/partial file matching so host
and remote path completion work. Auth-source uses the same authinfo files;
remote PATH is honored and autosaves are local.

Codex and Claude run directly through Ghostel/Sheprd, retaining the CLIs' existing
MCP configurations and credentials. The Doom setup declares no Emacs MCP client
or server, so none is invented here. `custom.el` in the state directory is loaded
last for machine-specific settings and additional existing integrations.

## Checks

After setup, from the repository:

```sh
# Use a temporary state directory, never the real one.
GLOOM_EMACS_STATE="$(mktemp -d)" /opt/homebrew/bin/emacs -Q --batch \
  -l configs/gloom/tests/config-test.el -f ert-run-tests-batch-and-exit
/opt/homebrew/bin/emacs -Q --batch \
  -l configs/doom/tests/sheprd-test.el -f ert-run-tests-batch-and-exit
```

The standalone checks exercise real persp-mode sessions/layouts, terminal
confinement, actual Evil editing/copying, the leader and native keymaps, code commands, TRAMP path parsing, remote server
selection and a real Ghostel native terminal. They also drive Sheprd's persistent agents end to
end (Claude, Codex and Pi in terminals, Pi in Pilish) against offline fake
clients in `configs/doom/tests`, which need `python3`; no account or network is
used. Remote network connectivity and
language server responses depend on the actual host/project.
