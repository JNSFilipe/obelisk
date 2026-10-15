#!/bin/bash
# Conventional Doom maintenance, always using the same Emacs as the daemon.
set -euo pipefail
export EMACS=/opt/homebrew/bin/emacs
doom_root="$HOME/.local/share/doom-emacs"
export DOOMDIR="${DOOMDIR:-$HOME/.config/doom}"
export DOOMLOCALDIR="$doom_root/.local"

if [[ ! -x "$EMACS" ]]; then
  echo 'Install the declared Emacs Plus cask first: brew install --cask d12frosted/emacs-plus/emacs-plus-app' >&2
  exit 1
fi

case "${1:-}" in
  install)
    if [[ ! -x "$doom_root/bin/doom" ]]; then
      if [[ -e "$doom_root/.git" ]]; then
        echo "Existing $doom_root is a git checkout without bin/doom; leaving it untouched." >&2
        exit 1
      fi
      # The Emacs launchd agent runs with --init-directory=$doom_root and
      # RunAtLoad, so on a fresh machine it creates this directory, and
      # DOOMLOCALDIR inside it, before Doom has ever been cloned.  Clone into a
      # staging directory and move the checkout in around whatever is already
      # there, rather than refusing to install or deleting the daemon's state.
      staging="$(mktemp -d)"
      trap 'rm -rf "$staging"' EXIT
      git clone --depth 1 https://github.com/doomemacs/core.git "$staging/core"
      mkdir -p "$doom_root"
      ( cd "$staging/core" && tar cf - . ) | ( cd "$doom_root" && tar xf - )
    fi
    if [[ ! -x "$doom_root/bin/doom" ]]; then
      echo "Existing $doom_root is not a Doom installation; leaving it untouched." >&2
      exit 1
    fi
    "$doom_root/bin/doom" -! install --no-config --no-env
    ;;
  sync) "$doom_root/bin/doom" sync ;;
  update) "$doom_root/bin/doom" upgrade ;;
  *) echo 'Usage: doom-manage.sh {install|sync|update}' >&2; exit 2 ;;
esac

# Prepare the official native module before a daemon has to prompt for it.
"$EMACS" --batch -Q -l "$DOOMDIR/install-native.el"
echo 'Doom is ready. Save your buffers, then restart Emacs to use the updated packages.'
