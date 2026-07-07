#!/usr/bin/env bash
# herdr-sessionizer: fzf + zoxide workspace picker for Herdr.
#
# Requirements:
#   herdr, zoxide, fzf, jq
#
# Optional env:
#   HERDR_SESSIONIZER_REUSE=0          # always create a new workspace
#   HERDR_SESSIONIZER_TITLE_STYLE=path # basename | parent | path
#   HERDR_SESSIONIZER_FZF_OPTS="..."   # extra fzf flags

set -euo pipefail

die() {
  printf "herdr-sessionizer: %s\n" "$*" >&2
  exit 1
}

need() {
  command -v "$1" >/dev/null 2>&1 || die "missing required command: $1"
}

need herdr
need zoxide
need fzf
need jq

pick_dir() {
  # Direct path or zoxide query:
  #   herdr-sessionizer ~/code/app
  #   herdr-sessionizer app
  if (( $# > 0 )); then
    if (( $# == 1 )) && [[ -d "$1" ]]; then
      cd "$1" && pwd -P
      return
    fi

    zoxide query "$@"
    return
  fi

  local fzf_args=(
    --prompt="herdr> "
    --height=80%
    --reverse
    --border
    --scheme=path
  )

  if [[ -n "${HERDR_SESSIONIZER_FZF_OPTS:-}" ]]; then
    # shellcheck disable=SC2206
    fzf_args+=(${HERDR_SESSIONIZER_FZF_OPTS})
  fi

  {
    pwd -P
    zoxide query -l 2>/dev/null || true
  } \
    | awk 'NF && !seen[$0]++' \
    | while IFS= read -r dir; do
        [[ -d "$dir" ]] && printf "%s\n" "$dir"
      done \
    | fzf "${fzf_args[@]}"
}

workspace_title() {
  local dir="$1"
  local base parent

  base="$(basename "$dir")"
  parent="$(basename "$(dirname "$dir")")"

  case "${HERDR_SESSIONIZER_TITLE_STYLE:-basename}" in
    basename)
      printf "%s\n" "$base"
      ;;
    parent)
      printf "%s/%s\n" "$parent" "$base"
      ;;
    path)
      printf "%s\n" "$dir"
      ;;
    *)
      die "invalid HERDR_SESSIONIZER_TITLE_STYLE; use basename, parent, or path"
      ;;
  esac
}

find_workspace_by_title() {
  local title="$1"

  herdr workspace list 2>/dev/null \
    | jq -r --arg title "$title" '
        .result.workspaces[]?
        | select((.label // "") == $title)
        | (.workspace_id // empty)
      ' \
    | head -n 1
}

create_workspace() {
  local dir="$1"
  local title="$2"

  herdr workspace create --cwd "$dir" --label "$title" --focus >/dev/null
}

main() {
  local dir title ws

  dir="$(pick_dir "$@")" || exit 0
  [[ -n "$dir" ]] || exit 0
  [[ -d "$dir" ]] || die "not a directory: $dir"

  dir="$(cd "$dir" && pwd -P)"
  title="$(workspace_title "$dir")"

  zoxide add "$dir" >/dev/null 2>&1 || true

  if [[ "${HERDR_SESSIONIZER_REUSE:-1}" != "0" ]]; then
    ws="$(find_workspace_by_title "$title" || true)"
    if [[ -n "$ws" ]]; then
      herdr workspace focus "$ws" >/dev/null
      exit 0
    fi
  fi

  create_workspace "$dir" "$title"
}

main "$@"
