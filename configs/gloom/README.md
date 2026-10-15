# Gloom Emacs

A parallel Emacs 30+ config written in plain Elisp. It shares the Doom config's
Sheprd implementation and Oxocarbon theme. It loads package libraries from the
existing Doom store, without starting Doom or Evil. `persp-mode` supplies sessions;
this deliberately uses the same package as Doom, rather than `perspective`.

## Run

From the repository, using the actual Emacs executable (the `emacs` shell wrapper
selects Doom):

```sh
/opt/homebrew/bin/emacs -Q --batch -l configs/gloom/install.el
/opt/homebrew/bin/emacs --init-directory="$PWD/configs/gloom"
```

The setup command installs only missing packages. On the current installation,
Meow is the only missing package. Setup also checks Ghostel's native module.
Normal startup never refreshes archives, downloads packages or installs grammars.
Meow is fetched from its official Git repository using Emacs's built-in package-vc.
Native compilation uses Gloom's own `eln-cache` state directory. The reused
Persp/Yas/Doom Snippets libraries load with JIT compilation disabled for that
dependency path because their source builds contain undeclared compiler calls.
Other libraries retain native compilation and its normal warning reporting.

Home Manager declares `~/.config/gloom-emacs` and the `gloom-emacs` launcher. They
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

`h j k l` move; uppercase motions extend the selection. `gg`/`G` jump to the
buffer's beginning/end; `0`/`^`/`$` move to line start/first text/end.
`i`/`a` insert/append at the selection; `I`/`A` insert at first text/line end,
and `o`/`O` open a line below/above. Escape returns to normal.

`v` toggles character selection; ordinary motions then extend it. `V` selects
lines, `w`/`W` select a word/symbol, and `b`/`e` extend word selections.
Select first, then use `d` to cut, `c`/`s` to change, or `y` to copy to **both the
kill ring and system clipboard**. `y` also copies ordinary marked regions in
read-only motion buffers. Insert state retains normal text typing.
`x` deletes a character, `p` pastes, `r` replaces a selection from the kill ring,
and `u`/`C-r` undo/redo. `/` searches with Meow Visit; `n`/`N` select the next/
previous occurrence. `1..9` keep Meow's selection expansion hints.

This remains selection-first: `w d` cuts the selected word; Vim's `dw` operator
grammar isn't added. `gr` grabs a selection and `gb` selects a block.
Meow's grab/beacon and keyboard macros handle repeated edits without adding a
second cursor package. See `SPC ?` for the configured cheatsheet and the
[Meow tutorial](https://github.com/meow-edit/meow/blob/master/TUTORIAL.org) for
beacon workflows.

Meow's leader is the native `C-c` prefix: `SPC t` and `C-c t`, for example, open
the same terminal. Unbound leader keys retain Meow's keypad behavior. `C-h` still
opens help in insert state; normal-state `C-h/j/k/l` focus adjacent windows,
including Ghostel terminals and the protected Sheprd sidebar. The sidebar also
binds these keys directly, so you can navigate back to the editor or terminal.
The cursor does not blink, including Ghostel terminal requests for blinking.

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
`a` launch Codex/Claude, and `?` help. Its sidebar requires graphical Emacs;
session and terminal commands also work in terminal frames. Ghostel starts in
insert state with its native shell keys. **Escape** enters normal state over
read-only scrollback, with terminal output still streaming; use `C-h/j/k/l` to
change panes and `y` to copy selected text while staying in normal state.
**i** resumes native terminal input at the live prompt. Native Copy/Emacs mode
commands and mouse selection also synchronize Meow's state. In explicit raw
char mode, Ghostel captures all keys; `M-RET` returns to semi-char input, where
Escape enters normal state. `M-x ghostel-send-next-key` forwards a literal
Escape to a terminal application. The sidebar retains its native buttons and
`j/k` item movement. Sheprd's `e` still opens editable scrollback in a separate
Meow buffer. Dired uses `h/l` up/open, `j/k` movement,
and its built-in marks, operations, `wdired` and remote support.

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

See [the parity audit](PARITY.md) for what is equivalent, what changes with Meow,
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
# Use a temporary state directory; point Emacs at the installed Meow library.
GLOOM_EMACS_STATE="$(mktemp -d)" /opt/homebrew/bin/emacs -Q --batch \
  -L "$HOME/.local/share/gloom-emacs/elpa/meow" \
  -l configs/gloom/tests/config-test.el -f ert-run-tests-batch-and-exit
/opt/homebrew/bin/emacs -Q --batch \
  -l configs/doom/tests/sheprd-test.el -f ert-run-tests-batch-and-exit
```

The standalone checks exercise real persp-mode sessions/layouts, terminal
confinement, actual Meow editing/copying and native keymaps, code commands, TRAMP path parsing, remote server
selection and a real Ghostel native terminal. Remote network connectivity and
language server responses depend on the actual host/project.
