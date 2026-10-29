# Sheprd: persistent agents with an M-x Machina–style UI

Status: implemented (awaiting the user's GUI trial)

## Why / scope
Bring M-x Machina's agent model and UI into Sheprd while keeping Sheprd's
sealed persp-mode sessions, and run Pi in Pilish. Sheprd stays one
self-contained file shared by Doom and Gloom. Machina is GPL-3.0-or-later and
this repository has no license, so its design is reimplemented as original
code; no Machina source is copied.

User decisions (2026-10-29): persistent registry with resume; each session is
the agents' folder; interfaces Ghostel (Claude, Codex, Pi CLIs) and Pilish
(Pi); remove Gloom's Machina mode afterwards.

## Behavior
Agents
- An agent is a named record: session, directory, client (claude, codex, pi),
  interface (ghostel, pilish), conversation ID, status, activity, unread,
  timestamps. Stored in SQLite under `sheprd-directory` (Emacs state dir).
- Creating an agent asks name, client, interface (Pi only), directory; it is
  claimed by the current session and starts at once. No worktree creation.
- Nothing starts on Emacs startup. RET on a stopped agent resumes the same
  conversation in its session; a failed resume is reported, never replaced by
  a new conversation. Stop keeps the record and buffer.
- Renaming a session moves its agents; killing a session stops its agents and
  asks whether to delete their records (default keep, shown as a closed
  session that RET reopens).
- Untracked terminals keep today's heuristic detection; `sheprd-track-agent`
  can register such a terminal as an agent.

Status (Machina's states, Sheprd's colors)
- working / approval (blocked) / input (ready) / stopped / failed, plus unread:
  a reply that finished while the agent was not on screen, cleared after it has
  been visible for a few seconds (replaces Herdr's `done`).
- Claude: per-run hook events (`--settings` JSON whose hooks append each event
  to a run file): prompt submit/tool use -> working, permission notification
  -> approval, stop -> input. Pilish: Pi RPC events. Codex and Pi in a
  terminal: Sheprd's existing text heuristics; Codex's `notify` hook also
  reports turn completion and its session ID.

Conversation IDs and resume
- Claude: new `--session-id <uuid>`, resume `--resume <uuid>`.
- Pi (Ghostel and Pilish): `--session-id <uuid>` for both (Pi saves a session
  only after its first message; an unused one resumes empty, reported).
- Codex: no ID before the first turn; captured from `notify`. Resume runs
  `codex resume <id>`; an agent stopped before its first turn cannot resume
  (reported).

UI
- Sidebar: current session expanded with its agents (state dot, unread `*`,
  name, client, branch); other sessions collapsed with working/waiting/unread
  counts. Mouse buttons kept. Keys: j/k, RET open/resume, n new agent, x stop,
  R rename, D delete, u mark read, ]/[ next/previous needing attention
  (approval first, then unread), z focus/restore, B board, i details,
  ? actions, TAB expand/collapse.
- Board: agents grouped by session in Working / Waiting / Ready / Stopped.
- Focus: an agent fills the frame inside its session; again restores.
- Prefix map (`C-c s`, `SPC TAB`): session and pane keys unchanged; agent keys
  gathered on free keys (exact table in sheprd.md).
- Pilish: chat above input inside the session; its buffers are sealed to the
  session like terminals.

## Approach
- Sections added to sheprd.el: registry (built-in sqlite), interface table
  (plist of start/stop/process/buffer functions per interface), observation
  (`sheprd--observe` agent status activity conversation), Claude hook bridge
  (shell `tr`/`cat` appends; file-notify with timer fallback), Pilish adapter
  (own code; Pilish internals isolated in one section and pinned by tests),
  sidebar/board rendering. Existing session, pane, sealing and heuristic code
  stays.
- Optional dependencies are declared, not required: Pilish loads only when a
  Pilish agent starts; sqlite is required for the registry.
- Gloom: delete machina.el, machina-pilish.el, the `gloom-sessions` toggle,
  Machina/EAT packages and tests; keep tests/fake-pi.py for Sheprd.

## Tasks (vertical slices)
- [x] 1. Registry + Ghostel interface: create/stop/resume Claude, Pi, Codex
  agents in sessions; sidebar rows from the registry.
- [x] 2. Claude hook status; Codex notify capture; unread/attention.
- [x] 3. Pilish interface (Pi RPC status, history on resume, sealed buffers).
- [x] 4. Board, focus, details/actions, keymap, notifications.
- [x] 5. Gloom cleanup (remove Machina mode); Doom wiring; sheprd.md.
- [x] 6. Tests: ERT with fake claude/codex/pi executables (no network), Doom
  and Gloom suites, strict byte-compile.
- [ ] 7. GUI trial by the user (Doom daemon restart picks up sheprd.el); close.

## Resolved
- antigravity-acp (Nix) and the pi-acp npm global removed with Gloom's
  Machina mode (user approved).
- Claude hook payload verified against Claude Code 2.1.308 (SessionStart with
  session_id = chosen ID). Codex notify fields (type, thread-id) read from the
  0.165 binary; a real turn was not run (needs a model call).

## Risks
Claude hook and Codex notify formats can change between CLI releases; Pilish
internals can change with Doom updates. Tests use fakes, so each slice ends
with a real-CLI smoke check without sending prompts where possible.

## Evidence
- Pending work committed and pushed first: bc0804f..6f1cdc9.
- Codex 0.165 hooks need per-hash "trust" (prompted every run), so Codex uses
  `notify` (agent-turn-complete / approval-requested with thread-id) plus
  screen heuristics. Claude: --session-id/--resume/--settings hooks.
- Slices 1-2 and the board are in sheprd.el (registry, runs, observation,
  terminal clients, Pilish adapter, commands, board, sidebar tree, keymaps).
  Strict byte-compile clean. sheprd-test.el: 51/51 (36 existing, 3 launch
  tests rewritten for sheprd-new-agent, 15 new incl. real hook/notify shell
  plumbing). Next: integration with real persp/Ghostel/Pilish and fake CLIs.
- 2026-10-29T18:32:00Z: integration. Fixes found by tests: tracked client and Ghostel's
  kill-on-exit must be set after/at Ghostel's mode change (ghostel-mode-hook
  keeps crashed transcripts); Pilish input window reuses the pane below or
  splits only with room; Gloom sets pilish-project-trust-policy `default`.
- Gloom Machina mode removed (files restored to e4ca6fc, machina*.el deleted,
  records archived as superseded); fake-pi.py moved to configs/doom/tests;
  fake-claude.py and fake-codex.py added.
- Checks: sheprd-test.el 51/51; strict byte-compile clean; Gloom suite 31/31
  (6 end-to-end agent tests); nix flake check passes.
- Real CLIs, no prompts: Claude 2.1.308 SessionStart hook with chosen ID after
  its folder-trust dialog; Codex 0.165 and Pi 1.4.0 start with Sheprd args;
  Pilish 3.4.0 live on --session-id; stop keeps buffers.
- Not verified: real Codex notify payload (needs a turn); GUI sidebar/board
  rendering and Doom daemon reload (user trial).
