# Sheprd

Sheprd is [Herdr](https://herdr.dev) rebuilt inside Emacs on persp-mode and
[Ghostel](https://github.com/dakra/ghostel). It maps Herdr's object model onto
what Emacs already has:

| Herdr | Sheprd |
| --- | --- |
| workspace | **session** — a persp-mode perspective, numbered 1…N |
| pane | **pane** — an Emacs window, normally a Ghostel terminal |
| agent | **agent** — a named, persistent coding agent owned by one session |
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

## Persistent agents

Agents are named records in a SQLite registry (`sheprd-directory`, under
Doom's data directory or `user-emacs-directory`), each owned by one session.
The design follows [M-x Machina](https://github.com/eliraz-refael/m-x-machina):
an agent outlives Emacs, and opening a stopped agent resumes the same
conversation in its session. A resume that fails is reported; it is never
replaced by a new conversation. Nothing starts on its own when Emacs starts.
Machina is GPL-3.0-or-later; Sheprd reimplements its ideas and copies none of
its code.

`prefix a` (or `n` in the sidebar) creates an agent in the current session:
choose the client, the interface for Pi, a directory and a name. It starts
at once. Directories are used as they are; Sheprd never creates worktrees.

| Client | Interface | Conversation | Status comes from |
| --- | --- | --- | --- |
| Claude Code | terminal | `--session-id`, resumed with `--resume` | per-run `--settings` hooks |
| Codex | terminal | its thread ID, reported after the first turn; `codex resume` | per-run `notify` program, plus the screen |
| Pi | terminal | `--session-id` for both | the screen |
| Pi | [Pilish](https://pilish.dev) | `--session-id` for both | Pi RPC events |

Claude's hooks and Codex's `notify` program only append JSON lines to a file
in the run's private directory (`runs/ID/`), removed when the run ends. Sheprd
never edits Claude, Codex or Pi settings. In a folder Claude has not seen
before, the agent stays `starting` until you answer Claude's trust dialog in
its terminal. A Codex agent stopped before its first turn has no conversation
to resume; opening it asks before starting a new one. Pi saves a session only
once it has messages, so a Pi agent stopped before its first prompt resumes
empty, and Sheprd says so.

Pilish agents show Pilish's chat with its input window below, and both
buffers are sealed into the session like terminals. Their status follows Pi's
RPC events; an open Pi extension dialog counts as `approval`.

Agent states: `working`, `approval` (waiting for you), `input` (ready),
`starting`, `stopped` and `failed`. Separately, an agent is **unread** (`*`)
when a turn finished while it was off screen; it clears after it has been on
screen for `sheprd-read-delay` seconds, or with `u`. Entering approval or
finishing unseen calls `sheprd-notification-function` (as `blocked` / `done`).

Stopping keeps the record, the conversation and the buffer, including the
transcript of a client that crashed. Renaming a session moves its agents.
Killing a session stops them and asks whether to delete their records; kept
ones appear under **CLOSED** in the sidebar, and opening one reopens the
session.

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

Agents:

| Action | Key |
| --- | --- |
| New agent in this session | `a` |
| Open or resume any agent | `A` |
| Next agent needing attention (approvals, then unread) | `!` |
| Pick an agent needing attention | `@` |
| Board: every agent by state | `O` |
| Stop / rename / describe an agent | `S` / `R` / `I` |
| Focus an agent; again restores the layout | `Z` |
| Open the last notification | `o` |
| Override a detected terminal's status | `s` |
| Unstaged diff of the agent at point | `d` |

The same agent commands are also on a leader prefix with M-x Machina's keys,
`sheprd-agent-map`: Doom binds it under `SPC a` (beside Pilish's `p`/`t`/`s`)
and mirrors that prefix on `C-c a`; Gloom binds it to `C-c a`, which is also
`SPC a`.

| Action | Key after `SPC a` / `C-c a` |
| --- | --- |
| Sidebar | `a` |
| New agent | `n` |
| Board | `b` |
| Pick any agent | `d` |
| Focus an agent | `z` |
| Next / previous agent needing attention | `]` / `[` |

Navigation and the client:

| Action | Key |
| --- | --- |
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

## Sidebar

Sessions form a tree. The current session is expanded with its agents (state
glyph, `*` when unread, name, then client and state); other sessions show a
summary of approvals `◍`, working agents `●` and unread replies `*`. Sessions
that exist only in the registry are listed under **CLOSED**. Terminals running
an agent Sheprd did not create are listed too, marked `(detected)`.

With the sidebar focused (`prefix f`):

| Action | Key |
| --- | --- |
| Move | `j` / `k` |
| Open or resume an agent; switch or reopen a session | `RET` |
| Fold or unfold a session | `TAB` |
| New agent | `n` or `a` |
| Stop / rename an agent | `x` / `R` |
| Delete the agent at point; on a session row, kill the session | `D` |
| Mark read | `u` |
| Next / previous agent needing attention | `]` / `[` |
| Focus / board / details | `z` / `O` / `i` |
| Refresh / hide | `g` / `q` |

The board (`O`) lists every agent by session in Working, Waiting, Ready and
Stopped columns; `RET` opens one.

## Layouts

Three layouts arrange the current session's editing area; the sidebar stays.
Each one toggles: applying it saves the session's layout, applying it again
restores that, and switching to another layout keeps the original to return
to. The saved layout belongs to the session (and frame), so every session
toggles its own. Layouts only use buffers the current session owns, fetching
them even when they are not on screen.

| Layout | Key after `SPC l` / `C-c l` | Arrangement |
| --- | --- | --- |
| Zoom | `z` (also `prefix z`) | The active buffer alone |
| LLM1 | `1` | The agent on top; the bottom third holds a terminal, or the prompt (input) buffer of a Pilish agent |
| LLM2 | `2` | As LLM1, the bottom third split: terminal or prompt on the left, the session's video on the right |

The agent is the one in the active pane, else the session's most recently
used agent, else the active buffer. The terminal is the session's most
recent terminal that is not an agent; if there is none, one starts in the
agent's directory. The video is the session's most recent ytube player
(`sheprd-layout-media-predicate`); without one, a placeholder explains how
to start one. Focus goes to the Pilish prompt, otherwise to the agent.
`sheprd-layout-bottom-fraction` sets the bottom row's height.

## Mouse

The sidebar is mouse-native like Herdr's: every session and agent is a button, so
`mouse-1` switches session, opens or resumes an agent, or reopens a closed
session.
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

**Detected terminals use text heuristics.** Agents Sheprd creates report
their status as described in [Persistent agents](#persistent-agents); Codex and
Pi in a terminal also read the screen. A terminal running an agent Sheprd did
not create is classified from its text alone, with Herdr's five states:

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

**Launching.** A new agent starts its client as a separate process through
Ghostel's public `ghostel-exec`; nothing is typed into an existing shell. The
terminal is claimed by the current session.

For aliases, wrappers or remote sessions that automatic detection misses, run
`M-x sheprd-track-agent` inside the terminal; `auto` clears explicit tracking.
Customize `sheprd-agent-commands` for client executables (Sheprd appends the
session and status arguments) and `sheprd-client-command-regexps` for names
recognized in detected terminals.

## Comparison with Herdr

| Capability | Sheprd |
| --- | --- |
| Workspaces / panes / prefix keymap | Implemented on persp-mode and Emacs windows |
| Numbered navigation, mouse-first sidebar | Implemented; digits select sessions |
| Tabs (layouts inside a workspace) | Deliberately omitted; `persp-switch-to-buffer` covers it |
| Agent state | Hooks (Claude), notify + screen (Codex), screen (Pi), Pi RPC (Pilish); heuristics for detected terminals |
| Find agents needing input | `prefix !` cycles approvals then unread replies across sessions; echo-area notifications, no desktop alerts |
| Zoom, swap, resize, copy mode, scrollback edit | Implemented (copy mode and scrollback come from Ghostel) |
| Review changes | Unstaged Magit diff in the agent's directory; use Magit for staged/untracked files |
| Keep running when closing a client | Only while the Emacs daemon and terminal processes stay alive |
| Restore after Emacs/OS restart | Agents are persistent; opening one resumes its conversation. Doom restores session layouts; processes are not kept running |
| Remote machines | Ghostel/TRAMP terminals and explicit tracking; no Herdr-style machine registry |
| Named server sessions, socket API, plugins | Not implemented |
| Automatic worktree provisioning / cleanup | Not implemented |

Sheprd is a local Emacs multiplexer, not a replacement for Herdr's independent
server. Persisting a Doom layout does not persist a PTY or agent session.
Running an Emacs daemon protects sessions from closing a GUI client, not from
daemon crashes, shutdowns or machine sleep. Use an external multiplexer if you
need terminal processes to survive Emacs termination; tracking then needs care.

## Integration and verification

Requires Doom `:ui workspaces` (or Gloom's persp-mode adapter), Ghostel with
`ghostel-exec`, Emacs built with SQLite for the agent registry, and a GUI frame
for the sidebar. Pilish is optional and loads only when a Pilish agent starts.
Sheprd uses Pilish's internal functions (`pilish--setup-session`,
`pilish--handle-event`, `pilish--apply-state-response`,
`pilish--load-session-history`) because Pilish has no embedding API; tests pin
them. Claude hook and Codex `notify` formats are the CLIs' own and can change
between releases. Config loads Sheprd in a
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

End-to-end agent tests run in Gloom's suite, against real persp-mode sessions,
Ghostel terminals and Pilish, with offline fake clients
(`configs/doom/tests/fake-{claude,codex,pi}.py`); see
[Gloom's checks](../gloom/README.md#checks).

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

On 2026-10-29, with persistent agents: 51 batch regressions and strict byte
compilation passed, and Gloom's suite (31 tests, 6 of them end-to-end agent
tests with the fake clients) passed. Against the real CLIs, without sending a
prompt: Claude Code 2.1.308 ran the per-run `SessionStart` hook with the session
ID Sheprd chose (after its folder-trust dialog); Codex 0.165 and Pi 1.4.0
started with Sheprd's arguments; Pilish 3.4.0 went live on `--session-id`.

Automated checks cover session ownership and sealing, split and close, per-frame
sidebar isolation, status transitions on real Ghostel output, process exit, and
the agent lifecycle against fake clients. What they cannot cover is a real
agent and a human at the keyboard. When convenient, in a healthy GUI session:

1. Create Claude, Codex and Pi agents in different sessions and watch their
   states while hidden, including that a hidden agent finishing shows `*`
   until it has been on screen; restart Emacs and resume each with `RET`.
2. Approve a request and verify the status changes; move with `!`, `]` and `RET`.
3. Split, zoom, resize and close panes; open `d`; expand a Vertico minibuffer
   with the sidebar showing.
4. Open a second GUI client, switch its session, close it, and toggle the
   sidebar.
