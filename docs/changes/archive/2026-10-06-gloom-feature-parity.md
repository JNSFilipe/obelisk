# Gloom Emacs naming and feature audit
Work ID: meow-emacs | Origin: independent apply | State: complete
Source: 2026-10-06-meow-emacs.md

## Scope
Rename the parallel config to Gloom Emacs, including source directory, launcher,
state directory and Elisp symbols. Audit actual enabled Doom modules and private
settings against the standalone implementation. Correct clear omissions from the
original request using existing packages; document equivalent behavior, intentional
Meow/UI changes, remaining gaps and verification limits explicitly.

## Tasks
- [x] Rename repository config, references, launcher and Elisp symbols.
- [x] Migrate the isolated installed package/state tree without data loss.
- [x] Audit enabled Doom behavior and address coding/integration omissions.
- [x] Test renamed startup, new behavior and graphical Sheprd integration.
- [x] Publish the parity matrix and verify rename references.
- [x] Archive the completed naming/audit follow-up and finalize its receipt.

## Evidence / Next
The previous 44 tests validated the core path, not exhaustive Doom parity.
Renamed configs/meow to configs/gloom, launcher/config link to gloom-emacs, state
to ~/.local/share/gloom-emacs and Elisp symbols to gloom-. The installed state
tree was moved without replacing existing data; Meow still loads successfully.

Restored Flycheck outside Eglot, Eros/Quickrun, language test/REPL providers,
Emmet and secondary web modes, Vertico Directory/Repeat, Consult Dir/Yasnippet,
Docker UI, generic new-file templates, Org/Babel/PDF wiring, native display rules,
macOS/TTY integrations and ligatures using installed libraries. Project-local
Node tools and Pyvenv locals precede Eglot discovery. Requirements mode defers
network fetching until completion. No extra Elisp packages were installed in
this follow-up.

The new tests caught the incorrect Eglot state predicate and a Yas setup timing
assumption; both corrected. The PDF failure was traced to an absent epdfinfo
binary in the shared Doom store, also affecting Doom. Lazy PDF link handlers and
an actionable viewer fallback keep ordinary Org and Cargo provider loading usable.

Current checks: all 17 Gloom ERT tests passed (Emacs Plus 31.1), including real
Python syntax diagnostics and native Ghostel output. A separate graphical Gloom
instance passed the shared Sheprd integration check and exited successfully.
Installer rerun succeeded without additional downloads. All new Elisp files
passed check-parens; both touched Nix files parsed; git diff --check passed.
Active config/launcher/docs references contain no old naming. The unchanged
36-test Sheprd regression result from the prior turn remains valid.

Reconciled user-facing baseline: configs/gloom/README.md, PARITY.md and docs/emacs.md.
PARITY.md records functional coverage and known differences. Naming/audit work
is implemented; **exact Doom parity is not achieved**. The full file-template
catalog and specialized Doom language/Org/lookup wrappers remain gaps; Meow and
native UI behavior intentionally differ. Package/native command equivalents
cover the essential workflows but do not reproduce every Doom helper.
No live remote hosts, language-server responses or MCP endpoints were exercised.
PDF rendering requires epdfinfo and native dependencies; Home Manager has not
been activated. These limits remain explicit rather than being called verified.
Naming and audit follow-up complete. Exact-wrapper parity remains as documented
in configs/gloom/PARITY.md; no claim of exhaustive parity is made.
