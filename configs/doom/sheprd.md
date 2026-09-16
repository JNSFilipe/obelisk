# Sheprd

Sheprd is [Herdr](https://herdr.dev) rebuilt inside Emacs on persp-mode and
[Ghostel](https://github.com/dakra/ghostel). It maps Herdr's object model onto
what Emacs already has:

| Herdr | Sheprd |
| --- | --- |
| workspace | **session** — a persp-mode perspective, numbered 1…N |
| pane | **pane** — an Emacs window, normally a Ghostel terminal |
| agent | **agent** — a recognized coding agent running in a pane |
| client/server | Emacs client frame attached to the Emacs daemon |
| sidebar | a left side window listing sessions and agents |

Herdr's **tabs are deliberately left out**: a second layer of saved layouts is
clutter when `persp-switch-to-buffer` already moves around inside a session. The
keys tabs would have taken go to sessions instead.

**Sessions are hermetically sealed.** Every terminal belongs to exactly one
session and is removed from every other perspective on each refresh. There is no
shared, global, or "scratch" terminal of any kind — that mechanism was deleted.
A terminal started outside Sheprd (`M-x ghostel`) is adopted by the current
session as soon as it opens.

## The prefix

Herdr reserves `ctrl+b` and sends the next key to the multiplexer. Sheprd does
the same with a single command map, `sheprd-command-map`, reachable two ways:

- `C-c s` everywhere, including inside a terminal pane — Ghostel lists `C-c` in
  `ghostel-keymap-exceptions`, so it reaches Emacs instead of the shell.
- `<leader> TAB` when Doom's leader exists (`SPC TAB` in normal state,
  `M-SPC TAB` from terminal insert state). Both the terminal `TAB` event and the
  GUI `<tab>` event are bound.

Change the global prefix with `sheprd-prefix-key` and the leader keys with
`sheprd-leader-keys`, then call `sheprd-install-keys`.

`prefix ?` lists everything.

## Keymap

Sessions. Herdr puts tabs on the plain digits; with tabs gone, sessions are the
numbered thing here, so digits select them.

| Action | Key |
| --- | --- |
| Switch to session 1–9 | `1`…`9` |
| Switch to the final session | `0` |
| Switch to the previous session | `` ` `` |
| Next / previous session | `n` / `p` (also `>` / `<`) |
| Session picker (completion) | `w` |
| Switch buffer within this session | `B` |
| New session | `N` |
| Rename session | `W` |
| Close session and its terminals | `D` |

Panes:

| Action | Key |
| --- | --- |
| New terminal in this pane | `t` |
| Split right / down | `v` / `-` |
| Focus pane left/down/up/right | `h` / `j` / `k` / `l` |
| Swap pane left/down/up/right | `H` / `J` / `K` / `L` |
| Cycle next / previous pane | `TAB` / `S-TAB` |
| Last pane | `;` |
| Close pane | `x` |
| Toggle zoom | `z` |
| Resize mode | `r` |
| Rename pane | `P` |
| Edit scrollback | `e` |
| Copy mode | `[` |

Agents, navigation and the client:

| Action | Key |
| --- | --- |
| Launch an agent | `a` |
| Jump to a blocked or done agent | `!` |
| Open the last notification | `o` |
| Override agent status | `s` |
| Unstaged diff of the agent at point | `d` |
| Goto picker (sessions, panes) | `g` |
| Toggle sidebar | `b` |
| Focus the sidebar | `f` |
| Refresh | `G` |
| Detach this client | `q` |
| Help | `?` |

Doom's own `M-1`…`M-9` still select workspaces, which are the same objects as
Sheprd sessions.

### Outside the prefix

`SPC t` opens a fresh terminal in the selected pane, `SPC ,` splits right into a
new terminal and `SPC <` splits down into one.

`C-x b` and `SPC b` are bound to `persp-switch-to-buffer`, not to
`switch-to-buffer` or `consult-buffer`. Both of those complete over every buffer
in the Emacs instance and would read straight through the session seal, listing
another session's terminals. `persp-switch-to-buffer` completes over the current
perspective only, which is the same boundary Sheprd enforces everywhere else.
`sheprd-switch-buffer` (`prefix B`) is the same command with a clearer error when
persp-mode is off.

## Mouse

The sidebar is mouse-native like Herdr's: every session and agent is a button, so
`mouse-1` switches session or opens an agent's pane.
Panes are ordinary Emacs windows, so clicking into one focuses it and dragging a
mode line resizes it.

## Semantics worth knowing

**Splits start terminals.** `sheprd-split-spawns-terminal` is `t`, matching
Herdr, where every pane is a terminal. A prefix argument inverts it for one
split, so `C-u prefix v` splits and keeps the current buffer.

**Closing a pane keeps a shared terminal.** A terminal dies with its last pane,
not with the first one you close, so closing half a split leaves the session the
other half is driving alone.

**Detach means closing a frame.** Herdr's `prefix+q` detaches a client and
leaves the server running. The Emacs analogue is deleting this frame while the
daemon keeps every process alive. Sheprd refuses when there is no daemon and no
other frame.

**Agent status is inferred from text, not from an agent protocol.** Sheprd
reports Herdr's five states:

| State | Meaning here |
| --- | --- |
| `working` | a working/interrupt indicator is the newest evidence |
| `blocked` | an approval prompt or a question at the prompt is newest |
| `done` | the agent stopped working while its pane was not on display |
| `idle` | at a prompt, and seen |
| `unknown` | no working, blocked or prompt evidence was recognized |

`done` clears as soon as the pane is displayed again, matching Herdr's
per-client seen tracking. Entering `blocked` or `done` calls
`sheprd-notification-function` once (echo area by default); `prefix o` jumps to
the agent that raised it. Quoted prompts, UI changes and large scrollback can
still confuse the heuristics — `prefix s` overrides a status and `auto` restores
inference. A shell command transition clears the override.

**Launching.** `prefix a` asks for the client and directory, then starts a
separate process through Ghostel's public `ghostel-exec`; it does not type into
an existing shell. The terminal is claimed by the current session. Directories
may be existing Git worktrees; Sheprd does not create or delete them.

For aliases, wrappers or remote sessions that automatic detection misses, run
`M-x sheprd-track-agent` inside the terminal; `auto` clears explicit tracking.
Customize `sheprd-agent-commands` for launchable clients and
`sheprd-client-command-regexps` for recognized executable names.

## Comparison with Herdr

| Capability | Sheprd |
| --- | --- |
| Workspaces / panes / prefix keymap | Implemented on persp-mode and Emacs windows |
| Numbered navigation, mouse-first sidebar | Implemented; digits select sessions |
| Tabs (layouts inside a workspace) | Deliberately omitted; `persp-switch-to-buffer` covers it |
| Working / blocked / done / idle / unknown | Implemented as text heuristics with seen tracking and overrides |
| Find agents needing input | `prefix !` across all sessions; echo-area notifications, no desktop alerts |
| Zoom, swap, resize, copy mode, scrollback edit | Implemented (copy mode and scrollback come from Ghostel) |
| Review changes | Unstaged Magit diff in the agent's directory; use Magit for staged/untracked files |
| Keep running when closing a client | Only while the Emacs daemon and terminal processes stay alive |
| Restore after Emacs/OS restart | Doom restores session state, not running agent processes |
| Remote machines | Ghostel/TRAMP terminals and explicit tracking; no Herdr-style machine registry |
| Named server sessions, socket API, plugins | Not implemented |
| Automatic worktree provisioning / cleanup | Not implemented |

Sheprd is a local Emacs multiplexer, not a replacement for Herdr's independent
server. Persisting a Doom layout does not persist a PTY or agent session.
Running an Emacs daemon protects sessions from closing a GUI client, not from
daemon crashes, shutdowns or machine sleep. Use an external multiplexer if you
need terminal processes to survive Emacs termination; tracking then needs care.

## Integration and verification

Requires Doom `:ui workspaces`, Ghostel with `ghostel-exec` (checked with
installed 0.52.0), and a GUI frame for the sidebar. Config loads Sheprd in a
daemon before GUI clients exist; frame hooks enable the sidebar when a GUI client
arrives. The sidebar buffer is per frame, and disabling removes timers, hooks and
windows. Disabling perspectives also disables Sheprd.

Ghostel's command/exit hooks are used without reading terminal text inside its
parser callbacks. Private `ghostel--pid`, process, identity and snapshot state
are isolated in detection/snapshot helpers. Hidden and copy-mode output uses
`ghostel--copy-all-text`; this copies scrollback before truncating, so very large
scrollback across many busy hidden terminals can cost time. If Ghostel removes
that API, buffer-text fallback may be stale; rerun checks after package updates.
No local process-tree inspection is used for TRAMP buffers.

Zoom captures the layout with `window-state-get` on the frame root with the
sidebar temporarily removed, so the side window is never stored inside a saved
layout. Session layouts are persp-mode's own, untouched by Sheprd.

Run the batch regressions from the repository root:

```sh
rtk proxy emacs --batch -Q -l configs/doom/tests/sheprd-test.el -f ert-run-tests-batch-and-exit
```

`tests/sheprd-integration.el` also provides `sheprd-ghostel-integration-check`
(native hidden terminal, requires the installed Ghostel dependencies on
`load-path`) and `sheprd-integration-check` (Doom GUI check). The latter requires
an already healthy GUI client, creates a temporary hidden frame and a harmless
shell, and cleans both up afterward. It covers session ownership, hidden-terminal
status, session-scoped buffer lists, cross-session button activation, per-frame
sidebar isolation, splitting and closing a pane, and process exit. It also sweeps
up the `#N` workspace Doom associates with the frame it creates.

On 2026-09-16 the 36 batch regressions, strict byte compilation (warnings as
errors), `sheprd-ghostel-integration-check`, `sheprd-integration-check` and a
live hermetic-sealing check all passed against the installed Doom and Ghostel on
Homebrew Emacs 31.1. Sealing was confirmed from a second session: the terminal
was absent from `+workspace-buffer-list`, from `persp-contain-buffer-p`, and from
the agent list, and `persp-buffer-free-p` was nil — no terminal is global.

Automated checks cover session ownership and sealing, split and close, per-frame
sidebar isolation, status transitions on real Ghostel output, and process exit. What they cannot cover is a real agent and
a human at the keyboard. When convenient, in a healthy GUI session:

1. Launch Codex and Claude in different sessions and watch status while hidden,
   including that a hidden agent finishing shows `done` until you open its pane.
   Real agent output, not the shell fixture, is what these heuristics are for.
2. Approve a request and verify the status changes; switch with `!` and `RET`.
3. Split, zoom, resize and close panes; open `d`; expand a Vertico minibuffer
   with the sidebar showing.
4. Open a second GUI client, switch its session, close it, and toggle the
   sidebar.
