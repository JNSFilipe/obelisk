# Plain Elisp Meow Emacs alongside Doom

Work ID: meow-emacs | Origin: independent apply | State: complete

## Scope and behavior

Add `configs/meow`, a standalone Emacs 30+ configuration using plain Elisp and
Meow. Preserve the Doom config and existing daemon. Reuse installed packages
without Doom startup/macros; install missing dependencies explicitly, never on
normal startup. Use the actual `persp-mode` package (not `perspective`).

Sessions own their buffers and window layouts; shared Sheprd/Ghostel code must
retain terminal confinement, agent status, sidebar and session commands.
Completion uses Vertico/Orderless/Marginalia/Consult/Embark; navigation uses
Dired and Projectile. Retain the same Oxocarbon theme, Eglot, snippets, code
modes, Git tools, compilation and agent CLIs (including their own MCP setup).
TRAMP supports built-in protocols, multi-hop, remote paths and local state.

## Approach

Load reusable Doom package build directories as libraries only. Prefer the
new config's own ELPA packages when present. Meow is the sole new core package;
use its selection, beacon and keyboard macro facilities for multiple editing.
Share Sheprd and theme files through sibling paths; implement a small persp-mode
adapter for the workspace calls used by Sheprd. Keep runtime state outside the
repository and outside Doom. Add a parallel launcher/config link to Home Manager.

## Tasks

- [x] Add isolated startup, package discovery and explicit dependency installer.
- [x] Implement Meow, workspace/Sheprd/Ghostel, completion, Dired/TRAMP and coding.
- [x] Add usage documentation and parallel Home Manager launcher.
- [x] Verify batch startup, workspace confinement/layouts, keymaps, code facilities
      and actual Ghostel native integration; run existing Sheprd regressions.
- [x] Reconcile documentation and close out with recorded evidence.

## Evidence / Next

Verified 2026-10-06 with Homebrew Emacs 31.1 and installed Meow 1.5.0
(revision 8aebed9f8cd8d865501b780e63acbeef55da231b):

- Eight standalone ERT scenarios passed, using a temporary state directory and
  the actual installed Meow package. Coverage: startup without Doom/Evil, real
  persp-mode layouts and terminal confinement, session rename/delete, file and
  layout persistence, native sidebar/terminal/Meow keymaps, Dired/TRAMP parsing,
  Eglot remote PATH selection and missing-server handling, all coding commands,
  and native Ghostel agent-output detection.
- All 36 existing Sheprd ERT regressions passed.
- A separate graphical Emacs launched through the documented --init-directory
  path and passed sheprd-integration-check: real Ghostel agent, session ownership,
  hidden-frame sidebar, sidebar button navigation, splitting and closing panes.
  The test instance closed after success; the Doom daemon was untouched.
- Explicit setup installed only Meow into the isolated local ELPA tree and
  verified the reused Ghostel native module. Repeat setup succeeded without
  downloads. ELPA archive hosts were unreachable; direct official GitHub
  installation worked.
- Emacs check-parens passed for every new Elisp source; git diff --check passed;
  nix-instantiate --parse passed for both affected Nix files.

Diagnosed/fixed missing treesit require, persp-mode's non-nil missing-name
sentinel, recursive Meow disable hook and sidebar-key reset after Sheprd reload.
The default Python server uses the repository's existing pylsp installation.

Baseline documentation reconciled in configs/meow/README.md and docs/emacs.md.
No reply arrived to optional MCP clarification; source declares no Emacs MCP
integration, so Codex/Claude retain their own existing CLI MCP settings.
External MCP endpoints, actual remote hosts and language-server responses were
not exercised. Home Manager launcher/link changes were parsed, not activated;
the direct launcher is ready now. These external acceptance checks are optional,
not unfinished implementation. Unrelated existing working-tree changes preserved.

Next: none. No pending implementation or closeout.
