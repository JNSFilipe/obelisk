#!/usr/bin/env bash
# cmux-sessionizer: fzf + zoxide workspace picker for cmux
#
# Requirements:
#   cmux, zoxide, fzf, jq
#
# Optional env:
#   CMUX_SESSIONIZER_REUSE=0          # always create a new workspace
#   CMUX_SESSIONIZER_TITLE_STYLE=path # basename | parent | path
#   CMUX_SESSIONIZER_COMMAND="nvim ." # run command in newly-created workspace
#   CMUX_SESSIONIZER_FZF_OPTS="..."   # extra fzf flags

set -euo pipefail

die() {
  printf "cmux-sessionizer: %s\n" "$*" >&2
  exit 1
}

need() {
  command -v "$1" >/dev/null 2>&1 || die "missing required command: $1"
}

need cmux
need zoxide
need fzf
need jq

pick_dir() {
  # Direct path or zoxide query:
  #   cmux-sessionizer ~/code/app
  #   cmux-sessionizer app
  if (( $# > 0 )); then
    if (( $# == 1 )) && [[ -d "$1" ]]; then
      cd "$1" && pwd -P
      return
    fi

    zoxide query "$@"
    return
  fi

  local fzf_args=(
    --prompt="cmux> "
    --height=80%
    --reverse
    --border
    --scheme=path
  )

  if [[ -n "${CMUX_SESSIONIZER_FZF_OPTS:-}" ]]; then
    # shellcheck disable=SC2206
    fzf_args+=(${CMUX_SESSIONIZER_FZF_OPTS})
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

  case "${CMUX_SESSIONIZER_TITLE_STYLE:-basename}" in
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
      die "invalid CMUX_SESSIONIZER_TITLE_STYLE; use basename, parent, or path"
      ;;
  esac
}

find_workspace_by_title() {
  local title="$1"

  cmux list-workspaces --json 2>/dev/null \
    | jq -r --arg title "$title" '
        .workspaces[]?
        | select((.title // "") == $title)
        | (.ref // .workspace_ref // .id // .workspace_id // empty)
      ' \
    | head -n 1
}

create_workspace() {
  local dir="$1"
  local title="$2"
  local output ws
  local args=(new-workspace --cwd "$dir")

  if [[ -n "${CMUX_SESSIONIZER_COMMAND:-}" ]]; then
    args+=(--command "$CMUX_SESSIONIZER_COMMAND")
  fi

  output="$(cmux "${args[@]}" --json 2>/dev/null || cmux "${args[@]}")"

  ws="$(
    jq -r '.workspace_ref // .workspace_id // .ref // .id // empty' 2>/dev/null <<<"$output" \
      || true
  )"

  if [[ -z "$ws" && "$output" =~ (workspace:[0-9]+) ]]; then
    ws="${BASH_REMATCH[1]}"
  fi

  if [[ -n "$ws" ]]; then
    cmux rename-workspace --workspace "$ws" -- "$title" >/dev/null 2>&1 || true
    cmux select-workspace --workspace "$ws" >/dev/null 2>&1 || true
  else
    # Fallback: cmux usually selects the newly-created workspace.
    cmux rename-workspace -- "$title" >/dev/null 2>&1 || true
  fi
}

main() {
  local dir title ws

  dir="$(pick_dir "$@")" || exit 0
  [[ -n "$dir" ]] || exit 0
  [[ -d "$dir" ]] || die "not a directory: $dir"

  dir="$(cd "$dir" && pwd -P)"
  title="$(workspace_title "$dir")"

  zoxide add "$dir" >/dev/null 2>&1 || true

  if [[ "${CMUX_SESSIONIZER_REUSE:-1}" != "0" ]]; then
    ws="$(find_workspace_by_title "$title" || true)"
    if [[ -n "$ws" ]]; then
      cmux select-workspace --workspace "$ws"
      exit 0
    fi
  fi

  create_workspace "$dir" "$title"
}

main "$@"
