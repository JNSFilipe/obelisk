# Daily-use Sheprd

Status: in-progress (GUI verification blocked)

## Why / scope
Strengthen the existing uncommitted Sheprd implementation without replacing
Doom workspaces or Ghostel. Match Herdr's local grouped-agent workflow: launch,
monitor, select an agent needing attention, and review changes. Herdr's independent
server, automatic agent resume after restart, remote machine registry, plugin/API
ecosystem, and worktree provisioning are architectural gaps, not promises of this
Emacs sidebar. Preserve the user's existing GUI-only rail and scratch integration.

## Behavior
Target: configs/doom/sheprd.md (usage and supported contract).
- Detect live shell-started and directly executed agents, including without OSC
  shell markers; do not identify grep arguments or stale banners as live agents.
- Recent working/approval/prompt evidence supersedes older output. Status remains
  heuristic, with explicit manual override for ambiguous or remote sessions.
- Launch a fresh named agent terminal in a selected directory/workspace; no
  command injection into an existing terminal. Jump to waiting agents globally.
- Load on daemon startup, enable on GUI frame creation, clean up on disable;
  preserve numeric workspace names, scratch ownership and panel selection.

## Approach
Use Ghostel's installed 0.52.0 public exec API and documented lifecycle hooks;
isolate private snapshot/process compatibility. Keep process probes paced and
avoid inspecting local process IDs for TRAMP terminals. Add batch ERT scenarios
and actual installed Ghostel integration checks where feasible.

## Tasks
- [x] Correct detection, snapshot freshness and status precedence; ERT regressions.
- [x] Add launch, explicit tracking/status and attention selection; ERT checks.
- [x] Correct daemon loading/frame lifecycle in code and document parity/limits;
  batch lifecycle checks pass. Real GUI frame behavior remains below.
- [ ] Verify installed Ghostel, byte compilation and GUI workflow; closeout.

## Evidence
- Original batch status reproduction returned `(waiting-input thinking)` for
  approval followed by working and working followed by an empty agent prompt;
  expected `(thinking stopped)`.
- Ghostel 0.52.0 installed in /nix/store; upstream sources checked 2026-09-08.
  Hook callbacks run synchronously inside parser before command-running changes.
  ghostel-exec does not emit shell integration markers.
- Existing user edits in sheprd.el/config.el and unrelated files preserved.

- `rtk proxy emacs --batch -Q -l configs/doom/tests/sheprd-test.el -f
  ert-run-tests-batch-and-exit`: 18/18 pass (2026-09-08). Covers status precedence,
  hidden/copy snapshots, shell-free exec, revalidation, process false positives,
  remote isolation, hook timing, explicit tracking, launch errors/argv, numeric
  workspace names, scratch routing, attention picker and timer cleanup.
- Strict byte compilation (`byte-compile-error-on-warn=t`, output in /tmp) passes.
  `check-parens` passes for sheprd/config and both test files. Scoped
  `git diff --check` passes; repository-wide check reports pre-existing unrelated
  Makefile line 29 whitespace (left unchanged).
- Actual installed Ghostel 0.52.0: added installed dependency directories to
  load-path, loaded tests/sheprd-integration.el, ran
  `(sheprd-ghostel-integration-check)` in separate batch Emacs: `passed`.
  A real native hidden terminal emits working then approval output; Sheprd reads
  the changed snapshot and removes it after process deletion. No paid agent run.
- GUI daemon test blocked before terminal launch. Stack sample
  `/tmp/sheprd-emacs-sample.txt` shows AppKit icon setup throwing a native
  exception, with Emacs waiting in fatal-signal delivery. User authorized restart;
  `launchctl kickstart -k gui/501/org.nix-community.home.emacs` completed, and
  `emacsclient --eval '(list (emacs-pid) (daemonp))'` returned `(90338 t)`.
- Supported `emacsclient -c -n` also failed: `could not get terminal name`.
  A standalone GUI attempt later blocked inside `Fload -> openp -> emacs_open`
  while opening the test file (sample `/tmp/sheprd-gui-sample.txt`), so this is
  distinct from the daemon's native frame failure. No GUI smoke success claimed.
  Temporary scratch text was removed; an interrupt was sent for the blocked file load (GUI recovery not confirmed).
- Final user files are edits in the working tree. The restarted daemon still
  uses its existing Nix-store config; changes have not been system-activated.

## Next
Resolve GUI startup/file-access problems before running the GUI smoke check and
manual steps in configs/doom/sheprd.md. The GUI smoke helper now requires an
already healthy GUI frame. Keep this record active per jig-close; real GUI
layout, cross-workspace interaction and multi-frame behavior are not verified.
Primary agent owns closeout; no workers, commits, deployment or archive.
