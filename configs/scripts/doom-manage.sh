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
    if [[ ! -e "$doom_root" ]]; then
      mkdir -p "$(dirname "$doom_root")"
      git clone --depth 1 https://github.com/doomemacs/core.git "$doom_root"
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
