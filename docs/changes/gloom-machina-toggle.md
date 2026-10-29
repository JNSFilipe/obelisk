# Gloom sessions toggle and M-x Machina

Status: implemented (awaiting the user's interactive trial)

## Why / scope
Let the user try M-x Machina (https://github.com/eliraz-refael/m-x-machina) in
Gloom without persp-mode/Sheprd interfering. Gloom only; Doom unchanged.

## Behavior
- `gloom-sessions` near the top of configs/gloom/init.el, default t (current
  behavior, unchanged). Changing it requires a restart.
- nil: persp-mode, the +workspace adapter and Sheprd are not loaded. Projects
  open with projectile-find-file; SPC b / C-x b use consult-buffer; SPC t/,/<
  open Ghostel terminals; C-c TAB is unbound. M-x Machina loads with EAT for
  Claude Code; `C-c a` (SPC a) prefix for its commands.
- Meow steps aside in Machina's list buffers and EAT terminals (native keys).
- install.el installs Machina (package-vc) and EAT (NonGNU) when missing.

## Tasks
- [x] Toggle in init/editing; machina.el; installer entries.
- [x] Tests for both toggle states; run installer in a temp state dir.
- [x] README docs.
- [ ] User trial of a real Claude agent through Machina/EAT in GUI Gloom; closeout.

## Evidence
- Machina 82151ab inspected: offers agent-shell profiles whenever agent-shell
  loads (Doom store has an untested build), so Gloom overrides
  mx-machina-agent-shell-configs and sets mx-machina-vterm-profiles nil.
- Installer into a temp state (/tmp/gloom-state): meow 8aebed9, mx-machina
  82151ab (package-vc, :lisp-dir lisp), eat 0.9.4 (NonGNU); Ghostel module ok.
- config-test.el, fresh temp state, sessions on (-L meow): 30 tests, 26 pass,
  4 skipped (Machina-only), 0 unexpected.
- Same with `--eval '(defvar gloom-sessions nil)'` and -L meow/mx-machina/eat:
  30 tests, 25 pass, 5 skipped (persp/Sheprd-only), 0 unexpected. Covers no
  persp/sheprd/+workspace, EAT-only profiles, fallback keys, C-c a map, Meow off
  in Machina views and eat-mode, real sidebar against a SQLite registry in the
  state dir, terminal split.
- check-parens clean for all edited .el files.
- 2026-10-29T09:44:46Z: same installer run for the real state (~/.local/share/gloom-emacs):
  meow, mx-machina, eat installed; Ghostel module ready.
- Not run: GUI Gloom session or a real Claude agent (needs the user).
- `gloom-emacs` launcher reads ~/.config/gloom-emacs (Nix store copy) until
  `make switch`; the repo-path command works immediately.
- User asked to default gloom-sessions to nil (init.el). Suite with real elpa: 25 pass, 5 skipped, 0 unexpected; real config batch load: machina=t persp=nil, claude/python3 found.
- Follow-up: worktree question now (y/N), RET = no (advice on the private
  mx-machina--read-new-session; covered by a test). agent-shell re-enabled,
  filtered to installed ACP commands (gloom-machina-acp-agents).
- Machina's own core+ACP suites against Doom's agent-shell 0.87.10 / acp 0.17.1
  (pinned/tested is 0.83.4): 103/103 pass, 0 unexpected (temp copy, offline).
- Gloom suite: sessions off 31 tests, 26 pass, 5 skipped; sessions on 26 pass,
  5 skipped; 0 unexpected. Real config lists codex, gemini-cli (agent-shell)
  and claude-eat (EAT).
- pi needs pi-acp (npm only; not Homebrew or nixpkgs). agy needs the
  Antigravity ACP server (not in the CLI cask; nixpkgs antigravity-acp 1.3.0,
  mainProgram agy_acp_server). Neither installed; awaiting user decision.
- User chose to install both adapters. pi-acp 0.0.34 (MIT) via Homebrew npm
  (`npm install -g pi-acp`, /opt/homebrew/bin/pi-acp); ACP initialize answered
  with loadSession. antigravity-acp 1.3.0 (unfree) declared in nix/packages.nix,
  allowlisted by name (allowedNixOnlyPackages) in flake.nix; built and its
  agy_acp_server(.par) answered ACP initialize with loadSession.
- `nix flake check`: 16 checks pass. Gloom suites (both states): 26 pass, 5
  skipped, 0 unexpected. Real config lists codex, gemini-cli, pi, claude-eat;
  with the Nix server on PATH (post `make switch`) antigravity is listed too.
- User clarified: skip the worktree question entirely. Advice now answers no
  without prompting (test: no prompt, nil worktree, other y-or-n-p untouched).
  Gloom suites both states: 26 pass, 5 skipped, 0 unexpected.
- `gloom` launcher added in nix/emacs.nix beside `gloom-emacs`; allowlisted in
  flake.nix. `nix flake check` passes; home.path bin/gloom verified.
- Pilish (Pi RPC porcelain, Doom store 3.4.0): no Machina extension point;
  an adapter would patch Machina internals (interfaces hard-coded across ~10
  files) and Pilish internals (setup/status/session-file). Decision pending.
