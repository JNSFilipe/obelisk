# Homebrew Emacs and conventional Doom

Status: in-progress

## Scope and behavior
User approved the ownership plan on 2026-09-09: Homebrew supplies GUI Emacs;
conventional Doom builds its packages with that Emacs; Nix declares installation,
configuration, client wrappers and one launchd service. Preserve existing Doom
configuration and the Nix installation until the replacement passes a trial.
Do not combine Homebrew Emacs with the Nix-built Unstraightened package bundle.

## Approach
Trial stable Emacs Plus alongside /Applications/Nix Apps/Emacs.app. Use isolated
Doom source/state and a named server. Check GUI accessibility/Tangrid, GUI and
TTY clients, native compilation and Ghostel/Sheprd. Then replace the Nix Doom
module and its app export with a Homebrew declaration, explicit launchd service,
matched Emacs/client/doom wrappers and conventional Doom bootstrap/update flow.
Preserve local edits and old package state. No brew-services duplicate daemon.

## Tasks
- [x] Install isolated Emacs Plus trial; verify GUI/client/accessibility capability.
- [x] Bootstrap conventional Doom with the user's modules and verify Ghostel.
- [x] Implement declarative ownership and migration/update commands.
- [ ] Validate Nix and shell configuration; activate only the verified replacement.
- [ ] Reconcile documentation and close with evidence and rollback instructions.

## Evidence
Homebrew Emacs Plus app 31.1-327 installed alongside the existing Nix app.
Conventional Doom core 01d68aaf6bd7db073365385cd82e1ad7e815295c installed at
~/.local/share/doom-emacs, with 184 packages synchronized using Homebrew Emacs.
Its source revision matches the previous Nix-locked core. Existing Nix runtime,
~/.config/emacs directory and rollback generations remain untouched.

Passed on Homebrew Emacs 31.1:
- Actual native compilation/load of a temporary function, returning 42.
- Bare and fully configured Doom GUI and TTY clients. TTY checks used `script`
  to supply a real PTY because RTK captures subprocess descriptors.
- All 19 ERT tests in configs/doom/tests/sheprd-test.el.
- sheprd-ghostel-integration-check and sheprd-integration-check returned passed;
  checked terminal status, workspace ownership, panels, agent button navigation
  and process exit. Confirmed temporary workspaces were removed afterward.
- macOS Accessibility exposes the real Doom window and window controls.
- Native Ghostel setup plus configs/scripts/doom-manage.sh sync (exit 0).
- Full Nix system build, locked variant build, Nix deadnix/statix check,
  ShellCheck, bash syntax check and git diff --check.
- Generated launchd label remains org.nix-community.home.emacs, in gui domain,
  with explicit Homebrew executable, Doom paths and complete tool PATH.
- Locked activation text contains HOMEBREW_NO_AUTO_UPDATE=1 and --no-upgrade;
  this replaces ineffective environment variables filtered by nested sudo.

First Ghostel GUI check stalled while displaying the missing-native-module
warning; a stack sample showed nested Lisp redisplay/recursive edit, not a
native macOS crash. Installing the official v0.50.0 module before the GUI
resolved it. The new maintenance script does this explicitly and verifies load.
All trial daemons were stopped after testing; the original daemon remains.

Nix path: snapshots initially hit disk limits by copying ignored legacy files.
Git-filtered snapshots succeeded; no user files or rollback generations deleted.
The two new Nix-relevant files were marked intent-to-add to make them visible to
Nix; their contents were not staged and nothing was committed.

Pending: actual Tangrid tiling could not be exercised through its sparse AX
interface. Doom doctor reports optional tool/font gaps documented in docs/emacs.md.
System activation was not performed: `sudo -n true` requires a password.
The original daemon had no modified file buffers or live terminal jobs at the
preflight check (only internal Messages/code-conversion buffers were modified).

## Next
User runs `make switch-locked` from the repository and enters their sudo password.
Then verify the default daemon's executable/version, config directory backup,
ec/et and Finder Emacs Client.app behavior, and Tangrid tiling. Keep the record
active until activation and acceptance checks pass. No worker or trial process
remains active. Documentation/rollback procedure is in docs/emacs.md.

## Activation follow-up (2026-09-09)
User's locked activation completed Homebrew but Home Manager stopped because
~/.config/emacs.bak-before-nix already existed. Inspection confirmed that backup
contains an older Doom checkout, while the current ~/.config/emacs contains only
.local and eln-cache. Preserved the older backup by renaming it to
~/.config/emacs.bak-before-nix.preserved-20260909-145111; verified its inode was
unchanged and the backup destination is now free. Current directory untouched.
No force-overwrite setting introduced. Activation retry still needs the user's
sudo password (`sudo -n true` returned password required).
