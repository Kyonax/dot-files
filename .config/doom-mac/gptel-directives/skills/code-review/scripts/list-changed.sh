#!/bin/bash
# Changed files with metadata -> JSON to stdout
# Usage: bash scripts/list-changed.sh [--base master]
#
# Combines staged, unstaged, and untracked files.  For each file emits:
#   { path, status, extension, directory }
# Plus a summary with counts per status and unique extensions.

set -euo pipefail

# ── args ─────────────────────────────────────────────────────────────────────
BASE="master"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --base) BASE="$2"; shift 2 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

# ── helpers ──────────────────────────────────────────────────────────────────
escape_json_string() {
  # Escape backslashes, double quotes, and control characters for JSON safety
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="${s//$'\t'/\\t}"
  s="${s//$'\n'/\\n}"
  s="${s//$'\r'/\\r}"
  echo "$s"
}

is_binary() {
  local filepath="$1"
  # Check by extension first (fast path)
  case "${filepath##*.}" in
    png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot|mp4|webm|mp3|ogg|pdf|zip|tar|gz|bz2|jar|class|o|so|dylib|dll|exe)
      return 0 ;;
  esac
  # Check file content if it exists
  if [[ -f "$filepath" ]]; then
    if file --mime-encoding "$filepath" 2>/dev/null | grep -q 'binary'; then
      return 0
    fi
  fi
  return 1
}

get_extension() {
  local path="$1"
  local base="${path##*/}"
  if [[ "$base" == *.* ]]; then
    echo ".${base##*.}"
  else
    echo ""
  fi
}

get_directory() {
  local path="$1"
  local dir="${path%/*}"
  if [[ "$dir" == "$path" ]]; then
    echo "."
  else
    echo "$dir"
  fi
}

# ── collect files ────────────────────────────────────────────────────────────
cd "$REPO_ROOT"

# Determine merge-base for diff (if base branch exists)
MERGE_BASE=""
if git rev-parse --verify "$BASE" >/dev/null 2>&1; then
  MERGE_BASE="$(git merge-base "$BASE" HEAD 2>/dev/null || true)"
fi

declare -A FILE_STATUS

# Staged files (vs HEAD)
while IFS=$'\t' read -r status filepath; do
  [[ -z "$filepath" ]] && continue
  case "$status" in
    A) FILE_STATUS["$filepath"]="added" ;;
    D) FILE_STATUS["$filepath"]="deleted" ;;
    M|MM) FILE_STATUS["$filepath"]="modified" ;;
    R*) FILE_STATUS["$filepath"]="modified" ;;
    *) FILE_STATUS["$filepath"]="modified" ;;
  esac
done < <(git diff --cached --name-status 2>/dev/null || true)

# Unstaged modifications (working tree vs index)
while IFS=$'\t' read -r status filepath; do
  [[ -z "$filepath" ]] && continue
  # Only set if not already tracked with a more specific status
  if [[ -z "${FILE_STATUS[$filepath]+_}" ]]; then
    case "$status" in
      D) FILE_STATUS["$filepath"]="deleted" ;;
      *) FILE_STATUS["$filepath"]="modified" ;;
    esac
  fi
done < <(git diff --name-status 2>/dev/null || true)

# Diff against base branch (files changed since divergence)
if [[ -n "$MERGE_BASE" ]]; then
  while IFS=$'\t' read -r status filepath; do
    [[ -z "$filepath" ]] && continue
    if [[ -z "${FILE_STATUS[$filepath]+_}" ]]; then
      case "$status" in
        A) FILE_STATUS["$filepath"]="added" ;;
        D) FILE_STATUS["$filepath"]="deleted" ;;
        *) FILE_STATUS["$filepath"]="modified" ;;
      esac
    fi
  done < <(git diff --name-status "$MERGE_BASE" HEAD 2>/dev/null || true)
fi

# Untracked files
while IFS= read -r filepath; do
  [[ -z "$filepath" ]] && continue
  if [[ -z "${FILE_STATUS[$filepath]+_}" ]]; then
    FILE_STATUS["$filepath"]="untracked"
  fi
done < <(git ls-files --others --exclude-standard 2>/dev/null || true)

# ── filter ───────────────────────────────────────────────────────────────────
declare -A FILTERED
for filepath in "${!FILE_STATUS[@]}"; do
  # Skip node_modules, .git, and binary files
  case "$filepath" in
    node_modules/*|.git/*|*/node_modules/*) continue ;;
  esac
  if is_binary "$filepath"; then
    continue
  fi
  FILTERED["$filepath"]="${FILE_STATUS[$filepath]}"
done

# ── counters ─────────────────────────────────────────────────────────────────
COUNT_MODIFIED=0
COUNT_ADDED=0
COUNT_DELETED=0
COUNT_UNTRACKED=0
declare -A EXT_SET

for filepath in "${!FILTERED[@]}"; do
  status="${FILTERED[$filepath]}"
  case "$status" in
    modified)  ((COUNT_MODIFIED++)) ;;
    added)     ((COUNT_ADDED++)) ;;
    deleted)   ((COUNT_DELETED++)) ;;
    untracked) ((COUNT_UNTRACKED++)) ;;
  esac
  ext="$(get_extension "$filepath")"
  [[ -n "$ext" ]] && EXT_SET["$ext"]=1
done

TOTAL=$(( COUNT_MODIFIED + COUNT_ADDED + COUNT_DELETED + COUNT_UNTRACKED ))

# ── emit JSON ────────────────────────────────────────────────────────────────
echo "{"
echo "  \"base\": \"$BASE\","
echo "  \"repoRoot\": \"$(escape_json_string "$REPO_ROOT")\","
echo "  \"files\": ["

FIRST=true
# Sort paths for deterministic output
SORTED_PATHS=()
while IFS= read -r p; do
  SORTED_PATHS+=("$p")
done < <(printf '%s\n' "${!FILTERED[@]}" | sort)

for filepath in "${SORTED_PATHS[@]}"; do
  status="${FILTERED[$filepath]}"
  ext="$(get_extension "$filepath")"
  dir="$(get_directory "$filepath")"

  if [[ "$FIRST" == true ]]; then
    FIRST=false
  else
    echo ","
  fi

  printf '    { "path": "%s", "status": "%s", "extension": "%s", "directory": "%s" }' \
    "$(escape_json_string "$filepath")" \
    "$status" \
    "$(escape_json_string "$ext")" \
    "$(escape_json_string "$dir")"
done

echo ""
echo "  ],"

# Summary
EXT_ARRAY="["
EXT_FIRST=true
for ext in "${!EXT_SET[@]}"; do
  if [[ "$EXT_FIRST" == true ]]; then
    EXT_FIRST=false
  else
    EXT_ARRAY+=","
  fi
  EXT_ARRAY+="\"$ext\""
done
EXT_ARRAY+="]"

cat <<ENDJSON
  "summary": {
    "total": $TOTAL,
    "modified": $COUNT_MODIFIED,
    "added": $COUNT_ADDED,
    "deleted": $COUNT_DELETED,
    "untracked": $COUNT_UNTRACKED,
    "uniqueExtensions": $EXT_ARRAY
  }
}
ENDJSON
