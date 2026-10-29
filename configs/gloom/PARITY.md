# Gloom / Doom parity audit

Audited on 2026-10-06 against `configs/doom/init.el`, `packages.el`, `config.el`,
the shared `sheprd.el`, and the enabled modules in the installed Doom source
(`~/.local/share/doom-emacs/sources/doom+/modules`). Commented-out modules and
packages are not requirements: Dape, Copilot, Company, Corfu and the LLM module
are disabled in this Doom configuration.

**This is functional coverage of the essential workflows, not exact Doom
feature parity.** The first implementation's core tests missed several package
integrations. Those have now been wired using the existing store; no Elisp
package needs installing beyond Doom's store. The table distinguishes shared behavior,
native/package equivalents, and remaining differences. “Configured” does not
mean that every external service or every package command has been exercised.

| Enabled Doom feature | Gloom implementation | Coverage / differences |
| --- | --- | --- |
| Workspaces / persp-mode | Same `persp-mode` package; thin `+workspace-*` adapter | Verified creation, switching, rename, deletion, file/layout persistence and terminal confinement. Own state; no import of Doom's saved sessions. |
| Ghostel / Sheprd | Loads the same `configs/doom/sheprd.el`; same Ghostel native module | Verified real terminal output and graphical sessions/panes/sidebar. Agent CLIs keep their existing MCP configuration; no live MCP calls tested. |
| Custom leader/window/terminal keys | C-c leader on SPC (normal/visual/motion) and M-SPC through an Evil intercept map; shared Sheprd map | C-h/j/k/l explicitly include the protected sidebar and terminal windows. The Doom config's Evil tweaks (visual-line j/k, $ to last non-blank, w/W select word, C-q, ç/Ç hunks) are kept. Ghostel uses evil-ghostel. C-c s stays Sheprd; vertical split uses C-c \| or Sheprd v. SPC D opens diagnostics; symbols remain SPC e s. Doom's general.el leader definitions and which-key labels are not reproduced; SPC ? lists the leader keys. |
| Evil +everywhere / multiple-cursors | Evil, evil-collection, evil-surround, evil-nerd-commenter, evil-ghostel, Pilish's pilish-evil | Vim operators, text objects, ex commands and evil-collection's special-mode keys. Doom's other Evil modules (snipe, easymotion, multiedit, evil-mc, args, lion, exchange, indent-plus, textobj-anyblock, visualstar, goggles, escape) are in the store but not enabled. |
| Vertico completion | Vertico, Orderless, Marginalia, Consult, Embark/Embark-Consult | Configured command/file/project/code completion, previews, actions/export, directory navigation, repeat, Consult Dir/Yasnippet and Wgrep. Doom-specific search/workspace narrowing wrappers are replaced by package commands and persp buffer filtering. |
| Dired | Built-in Dired/Dired-X/WDired, Diredfl, evil-collection keys with h/l up/open | File navigation and operations retained. Doom's Dirvish wrapper, history and subtree UI are not enabled. |
| Remote files | Built-in TRAMP, containers, remote PATH, authinfo and macOS Keychain | SSH/sudo/multihop/container paths parsed; remote Eglot executable discovery tested with a stub. No real remote host/container connection tested. |
| Oxocarbon / Doom UI | Same private `doom-oxocarbon` theme and Iosevka | Theme verified. Native mode line/help/display rules replace Doom UI. Doom quit messages, emoji command UI, Evil operation hints, symbol prettification and popup TTL rules differ. |
| Ligatures / hl-todo / vc-gutter | Ligature, hl-todo, diff-hl | Operators, TODOs, change gutter and hunk movement configured. Doom's expanded per-language pretty-symbol tables are omitted. |
| Syntax checker / Eglot | Flycheck in local non-Eglot code; Flymake under Eglot | Checker handoff verified. External checker binaries and actual language-server responses are environment dependent. Flycheck is disabled in remote buffers. |
| Spell | Built-in Flyspell, manual SPC - s | Matches the private config's manual activation. Requires an external spelling executable. |
| Evaluation +overlay | Eros for Elisp, Quickrun, eval-buffer/defun/region | Providers loaded and commands checked. Macrostep and Elisp Refs available. Doom's complete `+eval` dispatch/REPL-send wrapper API is not reproduced. |
| Lookup | Xref/Consult, Eglot, Eldoc, Helpful, Avy, FFAP, Dumb Jump fallback | Definition/reference/type/implementation/documentation commands configured. Doom's online/documentation-provider dispatcher and named docsets are not reproduced. |
| LSP +eglot | Built-in Eglot, Consult Eglot, completion preview | Servers discovered after file/directory locals and Node PATH setup. Existing installed grammars select native tree-sitter modes; fallback modes remain available. No package or grammar downloads at startup. |
| VC / Magit | Built-in VC, Magit, diff-hl, Git Link, Git Timemachine | Providers and commands available. Doom's custom VC command wrappers are replaced by the packages' commands. |
| Make / Docker / Terraform | Makefile Executor, compilation, Docker, Dockerfile/Terraform modes and Terraform Eglot | Providers loaded/commands checked. No build, Docker daemon or Terraform server interaction tested. |
| PDF | PDF Tools associations, HiDPI/fit-page, Saveplace PDF View, lazy Org PDF links | Correct wiring and graceful missing-helper behavior verified. The existing Doom store has no executable `epdfinfo`; rendering/annotations need a native build via `pdf-tools-install`. This also limits the current Doom install. |
| Snippets / file templates | Yasnippet + Doom Snippets, Consult Yasnippet, Auto Yasnippet | Existing snippets retained. Automatic new-file templates currently cover Python, Elisp, shell and Nix; the full Doom template catalog and its helper-dependent substitutions are a remaining gap. |
| C/C++ | Built-in CC/native tree-sitter modes, clangd, compilation, FFAP | Editing/navigation configured. Doom's mode-local key wrappers and naming/indent heuristics differ. |
| Python | Python modes, pylsp, Python 3 REPL, Pytest, Pyvenv, Pipenv, Nose, requirements mode | Package frontends retained; Pyvenv locals precede Eglot. Requirements files stay offline until completion is requested. Doom's IPython/Jupyter launch and Pipenv Quickrun dispatch wrappers remain different. |
| Go | Go/native modes, gopls, Gorepl, Go Tag/Gen Test, `go test ./...` via compilation | Providers retained. Doom's specialized single-test/benchmark/playground wrappers are not ported; use the package commands and compilation. |
| Rust | Rust/native modes, rust-analyzer; Rustic Cargo and Babel commands | Cargo check/build/test frontends retained without a competing LSP client. Explicit formatting replaces Rustic's automatic format hooks. |
| JavaScript/TypeScript | JS/TS/native modes, Node REPL, project `node_modules/.bin` | Project server lookup order verified. Gloom also allows TS language-server startup; Doom's javascript module has no +lsp flag. |
| Web | Web/HTML/CSS, JSX/TSX/Vue/Svelte, Haml/Pug/Slim/Sass/Stylus, Emmet/Rainbow | Existing mode libraries and associations reused. Doom's custom tag-navigation and web-mode key wrappers differ. |
| Lua / Nix / sh / Zig | Existing Lua/Nix/Zig and built-in shell modes; REPL/server/compilation commands | Providers configured. Doom's per-language command menus, help-dispatch wrappers and defaults are not copied verbatim. |
| Data / JSON / YAML | CSV/JSON/YAML modes and built-in configuration modes | Existing providers and native parser fallbacks retained. |
| Org | Built-in Org, native Babel/export/capture/agenda; TOC, Cliplink, async/Go/Rust Babel, PDF links, GFM/clipboard export and macOS link libraries | Org/PDF dependency isolation and Go Babel loading verified. Doom's extensive Org defaults, capture/export/heading helpers and Evil Org bindings are not reproduced. |
| macOS / TTY | Native frame/trash/auth settings, NS Auto Titlebar; Xterm Mouse, KKP, Xclip | Configured with existing packages. Graphical startup verified; terminal protocol/clipboard behavior not exercised in a live terminal frame. |
| Custom extras | Vundo, Consult GitHub, pixel scrolling, project roots, C-q/C-x b/C-x m | Retained. Consult GitHub authentication/API calls not exercised. |

## Evidence and limits

All 25 Gloom ERT tests passed with Emacs Plus 31.1. The suite exercises actual
persp-mode state, layouts and persistence;
Meow/native keymaps (Evil since 2026-10-29, see below); a real Ghostel terminal; command-provider loading; checker
handoff and a real Python syntax check; Node project executable lookup before Eglot; template expansion;
completion extensions; and Org/PDF failure isolation. A separate graphical run
passed the existing Sheprd integration check. The unchanged Sheprd suite previously
passed 36 tests. Nix parsing verifies syntax; Home Manager activation is separate.

The subsequent editing repair verified actual word selection/deletion, Vim-style
insertion/movement, marked-region copying, undo/redo and the native C-k fallback.
Normal and fresh-state graphical launches both verified macOS clipboard contents
using pbpaste. The fresh cache compiled 33 other files and neither warning-producing
Persp/Doom Snippets library; native compilation remained enabled and neither
reported compiler warning recurred. Those packages reuse existing bytecode.

The Sheprd navigation regression now exercises all four directions in a mixed
editor/native terminal layout with the real protected sidebar. It checks Meow
state transitions, continuing output, selected-text clipboard dispatch, PTY
keyboard input, and preservation of sidebar layout parameters. The earlier
ordinary-editor split check did not establish terminal/sidebar navigation.
Final checks passed 25/25 in batch and the focused navigation plus shared
integration checks in an initialized graphical frame. A fresh-state startup
recheck also hit an Emacs 31 native font crash while loading completion code;
its cause remains unestablished. That failed launch is not counted as passed.

On 2026-10-29 Gloom replaced Meow with Evil. The editing, navigation and
terminal tests were rewritten for Evil (operator grammar, insertion keys,
clipboard yanks, leader precedence, evil-ghostel states); with Sheprd's
persistent-agent tests the suite passed 31/31 in batch. Graphical behavior
under Evil (cursor shapes, clipboard via pbpaste, Claude Code's Escape
routing) has not been rechecked in a GUI frame.

Full parity cannot be inferred from these checks. The remaining gaps above are
mostly Doom-specific wrappers/defaults and the complete template catalog; the
UI differences are intentional consequences of the streamlined design.
External service checks still require actual hosts/projects/tools.
