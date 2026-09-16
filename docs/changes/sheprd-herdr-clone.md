# Sheprd as a full Herdr clone

Status: verified against installed Doom/Ghostel (batch regressions, strict byte
compilation, native Ghostel check, full GUI integration check and a live
hermetic-sealing check all pass; a real paid agent run is still not exercised)

Supersedes `harden-sheprd.md`, which hardened the earlier agent-sidebar-only
Sheprd and preserved the shared scratch terminal this change removes.

## Why / scope
Sheprd was a workspace-and-agent sidebar, not a multiplexer: it could group and
monitor agents but could not create tabs, split or manage panes, zoom, resize, or
be driven entirely from one prefix. It also shipped a scratch terminal that was
deliberately detached from every perspective and therefore reachable from all of
them, which breaks session isolation.

Goal: replicate Herdr's capabilities inside Emacs on persp-mode and Ghostel, with
hermetically sealed sessions and ergonomic control from both `C-c s` and Doom's
leader. Herdr's independent server, named server sessions, socket API, plugin
ecosystem, remote machine registry and worktree provisioning stay out of scope —
they are architectural, not missing keybindings.

## Behavior
Target: `configs/doom/sheprd.md` (usage and supported contract).

- Object model: session (perspective, numbered) → pane (window, normally a
  Ghostel terminal) → agent (recognized process).
- One prefix map, `sheprd-command-map`, on `C-c s` and on `<leader> TAB` (both
  `TAB` and `<tab>`). Ghostel passes `C-c` through, so the prefix also works from
  inside a terminal. `prefix ?` lists every binding.
- Herdr's letters where they do not collide: `v`/`-` splits, `h j k l` focus,
  `H J K L` swap, `z` zoom, `x` close pane, `r` resize, `[` copy mode, `e`
  scrollback, `n`/`p`/`w`/`N`/`W`/`D` sessions, `b`/`g`/`q`/`o` sidebar, goto,
  detach and notifications. Deliberate departures: plain `1`–`9` select sessions
  (Herdr numbers tabs there), and Herdr's tabs are omitted entirely.
- Hermetic sessions: every terminal records its owning session and is removed
  from every other perspective on each refresh. Terminals opened outside Sheprd
  are adopted by the current session. The scratch terminal, its minor mode, its
  perspective filter, its commands, leader bindings, tests and documentation are
  deleted outright.
- Agent states become Herdr's five — `working`, `blocked`, `done`, `idle`,
  `unknown` — with per-client seen tracking so an agent that finishes while
  hidden reports `done` until its pane is displayed. Entering `blocked` or `done`
  notifies once; `prefix o` jumps to the agent that raised it.
- Sidebar becomes one mouse-native side window with SESSIONS (numbered, with a
  per-session status rollup), TABS and AGENTS sections.
- `sheprd-detach` deletes the frame when a daemon or another frame exists, the
  Emacs analogue of detaching a client from Herdr's server.

## Approach
Keep the working parts of the previous module — Ghostel snapshot freshness,
paced process probes, TRAMP isolation, parser-callback safety, launch via
`ghostel-exec` — and build sessions/tabs/panes around them.

Session layouts stay persp-mode's own. Zoom captures and restores a layout with
the sidebar temporarily deleted, operating on the frame root, so the side window
is never serialized into a saved layout.

Herdr's own source was read for the canonical action list and defaults
(`src/config/model.rs`, `src/config/keybinds.rs`) rather than relying on the
website tables alone.

## Tasks
- [x] Rewrite `configs/doom/sheprd.el` around sessions, tabs, panes and agents.
- [x] Delete the shared scratch terminal and every reference to it.
- [x] Install the prefix map on `C-c s` and Doom's leader from the module itself;
  drop the workspace/scratch keybinding blocks from `config.el`.
- [x] Rewrite the batch regressions and the GUI integration check.
- [x] Rewrite `configs/doom/sheprd.md`.
- [x] Run `sheprd-integration-check` and the live sealing acceptance in the
  running daemon; fix what it found.

## Evidence
- `emacs --batch -Q -l configs/doom/tests/sheprd-test.el
  -f ert-run-tests-batch-and-exit`: 41/41 pass on Homebrew Emacs 31.1
  (2026-09-15). Covers status precedence and the five states, done/seen
  transitions, notification de-duplication, hidden/copy snapshots, shell-free
  exec, revalidation, process false positives, remote isolation, hook timing,
  explicit tracking, session numbering and sealing, terminal adoption, tab
  storage/switching/cycling/moving, pane predicates, sidebar button restoration
  and rollups, launch errors/argv/ownership, attention filtering, prefix-map
  coverage, prefix installation, timer/hook cleanup and detach refusal.
- Strict byte compilation with `byte-compile-error-on-warn=t` produces no
  warnings and a `.elc`.
- One real defect surfaced during the test run: the first sealing test used
  string perspectives, which are never `eq`; production compares perspective
  objects, so the test was interned to match real persp identity.
- Changes are working-tree edits; nothing is committed or Nix-activated.

## GUI and live verification — 2026-09-15

`~/.config/doom` resolves to this repository, so the running daemon already sees
the rewrite. A GUI frame was created from the daemon with `emacsclient -c -n`
(the AppKit failure recorded in `harden-sheprd.md` did not recur on emacs-plus).

- `sheprd-ghostel-integration-check`: `passed`. A real native hidden Ghostel
  terminal emits working then approval output, Sheprd reads the changed snapshot
  and reports `(codex blocked)`, and the agent disappears after process deletion.
- `sheprd-integration-check`: `passed` against installed Doom/Ghostel. Covers
  session ownership of a launched agent, blocked detection while hidden, per-frame
  sidebar isolation, tab creation and switching, absence from another session's
  buffer list, cross-session sidebar button activation, split/close-pane, and
  process exit.
- Live hermetic-sealing check on a terminal created in session one, inspected
  from session two: owner recorded `t`, listed in its own session `t`, listed in
  the other session `nil`, `persp-contain-buffer-p` in the other `nil`,
  `+workspace-buffer-list` of the other `nil`, `persp-buffer-free-p` `nil`,
  agent entries without a session `nil`, free terminals anywhere `nil`.
- Live keymap resolution: `C-c s` and `<leader> TAB` and `<leader> <tab>` all
  resolve to `sheprd-command-map`; `C-c s 1` and `<leader> TAB 1` both resolve to
  `sheprd-switch-session-index` with index 1.

### Two real defects the GUI run found

1. `sheprd--tabs` handed out a *fresh* default list when a session had no stored
   tabs, so `sheprd-new-tab` appended to a stale copy and discarded the layout
   `sheprd--save-current-tab` had just written. Switching back to the first tab
   showed the new tab's terminal instead of the pane that was left. The default
   list is now stored on first read, and `sheprd-new-tab` re-reads after saving.
   Regressions: `sheprd-new-tab-keeps-the-layout-of-the-tab-it-leaves` and
   `sheprd-default-tab-list-is-stored-not-regenerated`.
2. `sheprd-close-pane` killed a terminal that another pane was still showing, and
   asked `y-or-n-p` to do it. In the check that prompt landed on a hidden frame
   and wedged the daemon until `SIGUSR2`. A terminal now dies only with its last
   pane. Regression: `sheprd-close-pane-keeps-a-terminal-shown-elsewhere`. The
   integration check also answers prompts itself so it can never wedge a daemon
   again.

Batch regressions after both fixes: 41/41 pass; strict byte compilation still
produces no warnings; `check-parens` passes on all four elisp files.

### Daemon state

Verification terminals and the stale ` *sheprd-spaces*`/` *sheprd-agents*` panel
buffers from the previous design were removed, as were the GUI frame and the two
empty Doom frame-workspaces the run created. The daemon still holds in-memory
`+ghostel/scratch-toggle` and `+ghostel/here` definitions from before the file was
deleted; they disappear on the next daemon restart
(`launchctl kickstart -k "gui/$(id -u)/org.nix-community.home.emacs"`), which is
the user's call and was not performed.

### Still not exercised

A real Codex or Claude Code session. Status detection was verified against a
shell fixture that reproduces their working and approval output, not against a
paid agent run.

## Next
Restart the Emacs daemon when convenient so the deleted scratch commands leave
memory, then use Sheprd normally with a real Codex or Claude Code session to
confirm the `done` badge behaves as documented on live agent output. Nothing
blocks daily use before that.


## Follow-up — 2026-09-16

The user asked for Herdr's tabs to go: a second layer of layouts is clutter they
do not use, and moving around a session is better served by a buffer switcher.

- Removed tabs entirely: storage, commands, sidebar section, buttons, keys,
  help entries and goto-picker rows. Session layouts revert to persp-mode's own
  window-configuration handling; the layout helpers survive only for zoom.
- `n`/`p` now step through sessions (`<`/`>` kept as aliases). `M-1`–`M-9`,
  `c`, `T`, `X`, `{`, `}` are unbound, with a regression asserting they stay
  that way rather than lingering as dead keys.
- Added `sheprd-switch-buffer` on `prefix B`, and pointed `C-x b` and `SPC b` at
  `persp-switch-to-buffer` in `config.el`. This is a correctness fix, not only a
  preference: `switch-to-buffer` and `consult-buffer` complete over every buffer
  in the instance and list another session's terminals, reading straight through
  the seal Sheprd enforces everywhere else.
- Batch regressions: 36/36 pass (four tab tests removed, three added for the
  switcher and for the absence of tab keys). Strict byte compilation clean.
  `sheprd-integration-check` and `sheprd-ghostel-integration-check` re-run
  against the live daemon: both `passed`, with no workspace leak.

### Reload gap found while verifying

Checking the new keys in the running daemon showed the old map still answering:
`sheprd-command-map` was a `defvar` whose `let` body only ran the first time the
file was loaded, so re-loading sheprd.el left every removed key still firing its
old command. Both keymaps are now filled by `sheprd--populate-command-map` and
`sheprd--populate-sidebar-map`, which clear the map before rebinding and run on
every load, while keeping the map object identity the prefix and leader are bound
to. Verified live: after a reload `B` is `sheprd-switch-buffer`, `n`/`p` are the
session commands, `M-1`, `c`, `T`, `X`, `{`, `}` are all nil, and `C-c s` still
points at the same map.

Stale *function* definitions from a previous load (`sheprd-new-tab` and friends)
still linger until the daemon restarts; no amount of re-loading removes them.
`C-x b` and `SPC b` likewise take effect on the next config reload or restart.

### The stray perspectives were mine

The user found six perspectives open. Doom associates a fresh `#N` workspace with
every new frame, and the GUI verification created roughly six frames; deleting
them did not always remove the associated workspace. An earlier claim in this
record's GUI section that `#1`–`#5` pre-existed was wrong — there was no listing
from before the first frame was created, so they cannot be attributed to the user.

`sheprd-integration-check` now records the workspace list before it starts and
kills any empty workspace that appeared by the time it finishes; a re-run
confirmed it leaves the list unchanged. In the live daemon the empty `#2`, `#3`
and `#4` were removed. `#1` was left because it was the workspace the user's
frame was in, and `#5` was left because it holds a terminal the user started at
00:43 on 2026-09-16, well after the verification runs.


## ESC in normal state — 2026-09-16

The user reported ESC behaving strangely in Evil normal state. `config.el` bound
it two contradictory ways: `(map! :n "<escape>" #'keyboard-escape-quit)` a few
lines above the block whose comments say "Bind ESC and C-g together". The state
map wins, so ESC was never `doom/escape`.

`keyboard-escape-quit` ends its cond with "go back to just one window (by
deleting all but the selected window)" whenever more than one window is live, so
a stray ESC in normal state collapsed the editing layout — with Sheprd, every
pane but the selected one. The side window survived only because of its
`no-delete-other-windows` parameter.

- `:n "<escape>"` now binds `doom/escape`, the same command `C-g` runs. Insert
  and visual state keep their own ESC (`evil-normal-state`, exit visual).
- Removed `"C-[" #'doom/escape` from the `global-map` block. Emacs reads `C-[`
  as the ESC byte, so that binding replaced `global-map`'s ESC prefix map: every
  Meta key a terminal frame sends as ESC+key resolved to `doom/escape` instead.
  Verified before the fix that `(lookup-key global-map [27])` was `doom/escape`
  rather than `ESC-prefix`. `evil-esc-mode`, enabled by the `evil-esc-delay`
  setting already present, is what makes a lone ESC an escape in a terminal, so
  nothing is lost. `M-x` had survived only because it also sits in an override
  map.
- Both changes were applied to the running daemon as well, so the session is
  fixed before the next restart. Verified there: normal-state ESC and `C-g` both
  resolve to `doom/escape`, `(lookup-key global-map [27])` is `ESC-prefix` again,
  and `ESC f` / `ESC d` resolve to `forward-word` / `kill-word`.


## Config cleanup — 2026-09-16

Review of the rest of `config.el` turned up dead configuration; the user picked
three fixes and asked to keep the Doom template comments as they are.

- **Debug menu commented out.** The nine `SPC d` bindings called `dape`, which is
  not installed: `(package! dape)` is commented out in `packages.el` and
  `:tools debugger` is not enabled. Verified `(fboundp 'dape)` was nil, so every
  one of those keys errored. Left in place, commented, with a note naming the two
  steps needed to restore them.
- **`C-q` kills the current buffer in every state.** It used to be three
  commands: `evil-window-delete` in normal state, `evil-quoted-insert` in insert
  and replace, `kill-this-buffer` elsewhere. Doom's `'override' map was tried
  first and rejected: it does not outrank Evil's state maps, only maps that leave
  a key alone, so `M-x` worked there but `C-q` did not. The binding is now in
  `global-map` plus explicit `:i`/`:r` entries. Verified live across normal,
  insert, visual, replace, emacs and motion states — all `kill-this-buffer` — and
  `C-v` still inserts a literal character in insert state. Windows are still
  deleted with `C-w c`.
- **`C-x b` moved out of `(after! evil ...)`**, where it had nothing to do with
  Evil, to a top-level binding beside the Sheprd load that explains it.

While verifying, `C-x b` turned out not to run `persp-switch-to-buffer` at all:
Doom's vertico module binds `[remap persp-switch-to-buffer]` to
`+vertico/switch-workspace-buffer`, which is workspace-scoped and reaches other
workspaces only when you narrow to them explicitly. That is the better command,
so the binding stays and the comment now says so. `sheprd-switch-buffer` was
bypassing the remap by calling `persp-switch-to-buffer` directly, giving
`prefix B` a different picker from `C-x b`; it now goes through
`command-remapping` so both keys land on the same interface.

Not changed, reported only: the spell block's two `remove-hook!` lines are dead
(`:checkers spell` runs spell-fu, not flyspell, and spell-fu is still in
`text-mode-hook`), the 100MB `gc-cons-threshold` is inert under `gcmh-mode`,
`evil-disable-insert-state-bindings nil` restates the default, and
`consult-eglot-symbols` is labelled "LSP Diagnostics" though it lists symbols.
