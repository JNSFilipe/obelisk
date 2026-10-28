# YouTube inside Emacs

Status: complete

## Why / scope
Search YouTube from Emacs and watch inside an Emacs buffer (never an external
window). One self-contained .el file, loadable from any Emacs config, in the
style of sheprd.el. Elfeed/EMMS (bardman.dev) rejected: EMMS plays via external
mpv and has no search.

## Behavior
- Command prompts for a query; results in a completing-read (vertico) menu
  showing title, channel, duration and views; optional thumbnails (toggle).
- Selecting a result plays it in an xwidget-webkit buffer.
- yt-dlp is optional: when absent, search uses YouTube's web search endpoint
  via url.el; thumbnails come from i.ytimg.com and work either way.
- Requires GUI Emacs built with xwidgets for playback; clear error otherwise.

## Approach
Direct top-level youtube embeds in xwidgets fail with Error 153 (no referrer).
Serve a wrapper page from an Elisp localhost server (make-network-process) that
loads the embed via the IFrame API; the localhost origin satisfies YouTube.

## Tasks
- [x] Search backends (yt-dlp, built-in) with parsing tests.
- [x] Completion UI with annotations and async thumbnails.
- [x] Local wrapper server and xwidget player.
- [x] Doom integration (`SPC Y` / `C-c y` prefixes), byte-compile + ERT, live player check.
- [x] User accepted the result ("great, push and commit"); no separate report on the menu rendering.

## Evidence
- 2026-10-28: direct embed in xwidget showed "Error 153" (page text read via JS).
- 2026-10-28: localhost wrapper (python http.server) + IFrame API reached
  `ready` with no error; user confirmed Big Buck Bunny played in the buffer.
- Files: configs/doom/ytube.el, configs/doom/tests/ytube-test.el, config.el
  (`load!` + keys). No new packages; yt-dlp optional and not declared.
- Real yt-dlp output shape captured via ephemeral `uvx yt-dlp` (not installed):
  flat search has id/title/channel/duration/view_count, no upload date.
- `emacs --batch -Q` strict byte-compile (`byte-compile-error-on-warn`): clean.
- `emacs --batch -Q -l configs/doom/tests/ytube-test.el -f
  ert-run-tests-batch-and-exit`: 17/17 pass (parsing both backends, ID
  extraction, formatting, auto/fallback/explicit backends with fake yt-dlp,
  completion order/annotation/uniqueness, HTML escaping, live HTTP server).
- Live daemon (Emacs 31.1 NS): builtin search returned real results with all
  fields; oEmbed lookup resolved Big Buck Bunny title/channel; `ytube-play`
  played with sound (state 1, unmuted), buffer renamed to the title, Evil
  emacs state; pause/seek/volume/speed commands verified via player state;
  thumbnails swapped from placeholder to 64x36 JPEG in place within 3 s;
  `SPC Y s`, `C-c y s`, `C-c y p` resolve.
- Not live-tested: real yt-dlp backend (not installed).
