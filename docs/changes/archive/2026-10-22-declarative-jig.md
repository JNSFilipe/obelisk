# Declarative Jig and shared skills

Work ID: 20261022T213330Z-declarative-jig
Origin: independent apply
State: complete

## Scope and decisions

The user wants every currently installed Pi skill/extension declared for both Macs, with Pi owning Jig instead of standalone copied installations. Confirmed scope: Pi only; unrelated Claude plugins stay unchanged. Claude Code and Codex must keep Jig access by linking to Pi's single installed copy.

- Declare `git:github.com/JNSFilipe/jig` alongside the existing three npm packages in `nix/pi.nix`; float versions as before.
- After Pi reconciliation, link installed `jig-*` skill directories into `~/.claude/skills` and `~/.codex/skills`. Do not link into `~/.agents/skills`, which Pi scans and would discover twice.
- Preserve writable Pi settings and all non-package settings, credentials, sessions, unrelated skills/plugins, and the developer Jig checkout.
- Sharing is idempotent and preflights all collisions; never automatically delete unmanaged directories or foreign links. A missing Jig checkout defers sharing without network access on locked activation. An installed checkout with no Jig skills is an error.
- Perform the explicitly authorized local migration only after installing/validating the package, checking all 22 manual directories match the clean canonical source checkout, and verifying the replacement skills. Remove those manual directories and create the shared links. Do not create backup copies or remove unrelated/archived material.
- No system activation, commit, or push is authorized by this request. Targeted user-level Pi installation/migration is authorized.

## Tasks

- [x] Add shared package and skill-link activation, with isolated regression tests for idempotence, collisions, missing sources, unrelated resources and every host's normal/locked activation.
- [x] Install Jig locally, verify source skill discovery offline, remove verified manual copies, and share the installed skills with Claude/Codex.
- [x] Verify all declared/installed Pi resources, settings preservation, and no duplicate Pi skill names; run `make check-pi`.
- [x] Update README as the existing behavior baseline (no docs/specs directory), reconcile migration/locked behavior, and archive this record.

## Evidence

Initial inventory: `pi list` and `npm ls --depth=0` show pi-web-access 0.44.0, @juicesharp/rpiv-ask-user-question 2.13.9 and @heyhuynhgiabuu/pi-task 0.12.0. Eleven manual Jig directories in each of ~/.agents/skills and ~/.claude/skills exactly match the local canonical skills checkout (ignoring .DS_Store). No standalone Pi skills/extensions directories. Initial obelisk working tree clean. Independent repository exploration could not start: declared `memory` subagent skill is unavailable.

### Implementation and migration (2026-10-22)

- Added `git:github.com/JNSFilipe/jig` to the shared manifest. Added `configs/scripts/pi-share-jig.py` after package reconciliation, with fail-safe preflight, deferred sharing when a locked host has no checkout, malformed/empty installed-source errors, idempotent links, and pruning restricted to stale helper-owned symlinks.
- `make check-pi` passed after final code edits: both system builds, static analysis/package ownership checks, nine settings-merge tests, ten skill-sharing tests and all four normal/locked generated activations. Fresh normal activation tests create the mock Git checkout during Pi update, establishing that sharing runs afterward. Dry runs leave settings/link destinations untouched; normal updater failures propagate; locked activations run no updater.
- `nix fmt -- --fail-on-change flake.nix nix/pi.nix` passed (no changes), as did `git diff --check`.
- Native `pi install git:github.com/JNSFilipe/jig --no-approve` installed published revision `f091f1b3206eb4aa2c66927cff9d7b3444461f3f`. Isolated Pi RPC discovered all 11 Jig skills before deletion, with extensions disabled and no model prompt.
- Rechecked both Git checkouts were clean, checked all 22 manual directories still exactly matched the old canonical checkout, preflighted empty Codex destinations, then removed only those directories. Created eleven Claude links and eleven Codex links directly to the Pi checkout. Repeated the helper successfully. No backup copies were made; the developer checkout, unrelated/synced skills, archives and Claude plugins remain intact.
- Migration left Pi settings byte-identical and auth/model/session metadata and Claude plugin-registry metadata unchanged. Final live settings match the generated Nix manifest's four packages. Final Pi RPC metadata check with the installed extensions enabled discovered exactly 12 distinct skills (11 Jig plus the dynamically registered `task` skill), with no stderr warnings, no model prompt, no persisted session and unchanged settings.
- Isolated Codex 0.163.0 app-server `skills/list` verified discovery of a Pi-managed skill through a `~/.codex/skills` symlink with user scope and enabled=true. Official Claude skills docs confirm directory-symlink support: https://code.claude.com/docs/en/skills . Codex general docs now emphasize `~/.agents/skills`; actual installed-client verification establishes support for the separate user directory without adding Pi duplicate-discovery paths.
- Independent explore/scout/reviewer could not run because the agent definitions reference unavailable `memory`, `source-driven-development`, `code-review-and-quality` and `verification-before-completion` skills. Parent performed mapping, documentation verification, diff review and tests; no independent review is claimed.
- Initial new unit-suite failure was only macOS's `/var` to `/private/var` alias: resolve both compared paths. An initial formatter invocation used unsupported `--check`; corrected to treefmt's `--fail-on-change`. Initial final-discovery probes exposed shell-token parsing of line continuations and the fact that `--no-extensions` also suppresses pi-task's dynamically registered skill; corrected the verification procedures without changing working product behavior.

## Remaining limitations

No system activation, commit or push was performed. Current-machine user-level installation and migration are complete; other machines receive configuration at their next activation and require explicit migration if unmanaged Jig copies exist. Existing clients must reload skills or start new sessions. Existing pi-task agent-delegation errors for missing declared skills predate this change and are outside this installation/migration scope.

## Next

None. Configuration and local user-level migration are verified and complete; no system activation or publication requested.
