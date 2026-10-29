# Pilish as an M-x Machina interface (Gloom)

Status: implemented (awaiting the user's GUI trial)

## Why / scope
In Gloom with `gloom-sessions` nil, Pi agents created in M-x Machina should run
in Pilish (Pi's native Emacs UI over Pi RPC) instead of agent-shell + pi-acp,
while keeping Machina's sidebar status, focus, stop and resume. Gloom only;
no upstream changes. The pi-acp/agent-shell Pi profile stays available.

## Constraints found (Machina 82151ab, Pilish 3.4.0 from Doom's store)
- Machina has no interface extension point: agent-shell/EAT/vterm are
  hard-coded in backend configs/start/process/stop/metadata, messaging,
  recovery and diagnostics (agent-shell is usually the fallback branch).
- Pilish has no hooks or public start/status API. Needed pieces are private:
  `pilish--setup-session` (named sessions per directory), buffer-local
  `pilish--status` (idle/sending/streaming/compacting), `pilish--state`
  (:session-file, :model), `pilish--handle-event` (all RPC events),
  `pilish--resume-selected-session`. Public: `pilish-open-session-file`.
- Machina shows one conversation buffer in the main window
  (`mx-machina--show-conversation`); Pilish needs chat + input windows.

## Behavior
- New profile `pi-pilish` (Agent: Pi, Interface: pilish) listed when Pilish
  and `pi` are available.
- RET/start: a uniquely named Pilish session in the agent's directory; the
  chat buffer is Machina's conversation buffer, input window shown below it.
- Sidebar status: starting until Pi reports its session file, then live;
  activity `working` while sending/streaming/compacting, `approval` while an
  extension UI request is pending, otherwise `input`; `stopped` on exit.
- Conversation identity = Pi session ID, chosen by Gloom and passed as
  `--session-id`; resume passes it again and renders history when Pi has
  messages. A different reported ID fails the run (no replacement).
- Stop ends the Pi process without prompts, keeping buffers for inspection.
- Events for unread/attention: prompt, message, turn-ended, metadata (model).

## Approach
`configs/gloom/machina-pilish.el`, loaded by machina.el, using :around/
:filter-return advice on the Machina dispatch functions (configs, start,
process, stop, metadata, show-conversation, diagnostics/recovery only where
they would break) and :after advice on `pilish--handle-event` to translate
Pilish state into Machina's transport callback and event hook. Messaging to
Pilish agents is out of scope for v1 (clear error). Load Pilish in Gloom with
Doom's settings (Homebrew `pi`, default trust policy, no Evil).

## Tasks
- [x] Load Pilish in Gloom; Meow policy for chat/input buffers.
- [x] Profile listing and start/stop/process/metadata dispatch.
- [x] Status/event translation; session identity and resume guard.
- [x] Two-window display through Machina's show/focus.
- [x] Offline tests with a fake `pi --mode rpc`; Gloom suites in both states.
- [x] Real Pi startup/stop through the adapter (no prompt sent); docs.
- [ ] User GUI trial with real prompts; closeout.

## Risks
Private APIs on both sides: a Pilish or Machina upgrade can break the
adapter. Tests pin the touched surfaces so breakage shows up as failures.
Gloom reuses Doom's Pilish build, so a Doom update also upgrades it here.

## Decisions during implementation
- Pi CLI `--session-id <id>` (exact ID, created if missing) replaces Pilish's
  private resume path. Probe (temp --session-dir): Pi writes a session file
  only after the first message; `--session <id>` then fails with "No session
  found", so resume uses `--session-id` and reports an empty resume instead.
- Pilish answers extension dialogs synchronously inside its event handler;
  :around advice marks approval exactly while the dialog is open.
- Machina messaging: `mx-machina-messaging--hold` returns a reason for Pilish
  agents, so their messages stay queued instead of failing delivery.
- Pilish's chat uses md-ts-mode, which asks to install the Markdown grammar
  (none installed on this machine); kept as a prompt per Gloom's no-download
  policy; tests bind treesit-auto-install-grammar to never.
- pilish added to gloom-packages (installed from MELPA only if Doom's copy is
  missing); trust policy `default` as in Doom.

## Evidence
- configs/gloom/machina-pilish.el (adapter), machina.el (load),
  packages.el, tests/fake-pi.py, tests/config-test.el (4 tests), README.
- Fake-Pi ERT (offline): internals pinned; lifecycle (session-id argv, live/
  input, model, working then unread, approval during a confirm dialog with
  extension_ui_response sent, stop keeps buffer, resume same ID + get_messages);
  different-ID run fails; open shows chat above selected input; messaging hold.
  4/4 pass.
- Gloom suites: sessions nil 35 tests, 30 pass, 5 skipped; sessions t 26 pass,
  9 skipped; 0 unexpected.
- Real `pi` (no prompt): live/input, conversation = passed UUID, model
  "Claude Opus 5.5 1M", argv (pi --mode rpc --session-id <uuid>); stop ->
  stopped with buffer live.
