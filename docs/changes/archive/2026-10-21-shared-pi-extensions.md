# Shared Pi extension declarations

Work ID: 20261021T183544Z-pi-extensions
Status: complete (configuration verified; not activated)

## Why / scope
Declare the existing Pi packages on both Macs while retaining the Homebrew Pi binary and writable, machine-local settings. Do not activate, install extensions into the live agent, commit, or change credentials/session data. Preserve the existing user edit to flake.lock.

## Behavior
Baseline: [README Pi extensions](../../../README.md#pi-extensions).
- Add a shared list: npm:pi-web-access, npm:@juicesharp/rpiv-ask-user-question, npm:@heyhuynhgiabuu/pi-task (unpinned, matching existing use).
- Activation replaces only the `packages` key in ~/.pi/agent/settings.json. Preserve all other settings, never copy device identity or credentials into Nix, and leave settings writable. The declared list is authoritative; cached removed packages are not deleted.
- On missing settings, create them. On invalid JSON or a non-object root, fail without changing the existing file. Use atomic replacement and retain existing permissions. Reject symlink settings rather than overwriting an unrelated managed target.
- Regular activation reconciles/updates global packages through /opt/homebrew/bin/pi update --extensions, with project trust explicitly denied and a known working directory. Command errors fail activation rather than silently claiming success.
- Locked activation merges declarations but does not invoke package installation/upgrades. Pi installs missing packages on next startup. Respect Home Manager dry runs (no writes or network).
- Preserve auth.json, sessions, caches and independent model/provider preferences.

## Approach
Use a separate shared nix/pi.nix Home Manager module with a JSON manifest generated from a Nix list. A small Python standard-library script safely merges only that key using Homebrew Python. The parent Darwin configuration's Homebrew upgrade policy also controls extension updates (false on locked variants). Document normal/locked behavior in README. Add isolated script unit tests and evaluate generated activation text on every host/variant; build both systems and static/ownership checks.

## Tasks
- [x] Implement atomic settings merge and focused isolated tests.
- [x] Integrate shared Pi module and locked update policy.
- [x] Verify focused tests, all four configurations, dry-run integration, flake checks and diff hygiene.
- [x] Reconcile docs, close and archive this record.

## Evidence / Next
- Installed Pi 1.3.0 docs and package-manager/settings-manager sources confirm global package reconciliation, command-first CLI syntax, and proper-lockfile's settings.json.lock convention. The helper cooperates with this lock and never steals busy locks.
- PASS `make check-pi`: both Darwin systems, Homebrew ownership check, nine isolated merge tests in Nix, deadnix/statix, and generated activation tests for gauss/thorp plus both locked variants. Integration verifies declaration replacement, preference preservation, dry-run no-op, explicit global config directory, project trust denial, normal update execution/error propagation, and no locked update.
- PASS `/opt/homebrew/bin/python3.13 tests/pi-packages.py`: nine tests, including malformed settings/manifest, symlinks, busy locks, atomic replace failure, idempotence, permissions, auth/session preservation, and paths containing spaces.
- PASS Black formatting, Ruff, Nix formatting and `git diff --check` (final code, 2026-10-21).
- PASS real Homebrew Pi CLI smoke test with isolated HOME/config, empty package list and offline mode: `pi update --extensions --no-approve` accepted, no packages/network/live state involved.
- Initial tests exposed missing module-argument defaults and ineffective per-key extraSpecialArgs overrides (Home Manager uses a non-recursive attrs type); final integration instead reads the resolved parent Darwin Homebrew upgrade policy and verifies both locked variants. Test harness incompatibilities with Apple Bash 3 and /var-to-/private/var canonicalization were reproduced and corrected without changing production behavior. Initial lint findings were resolved and rechecked.
- Delegation unavailable: general and reviewer task launches failed because declared agent skills are missing. No child ran or changed files; primary implemented and reviewed directly.
- Pre-existing flake.lock update remains unchanged. No live activation, extension download, credential/session edit, commit or push performed. Network extension-install behavior is supported by Pi source, not an actual live installation test.
Next: none. Apply with `make switch` when ready; this performs the network extension reconciliation. `make switch-locked` only merges declarations.
