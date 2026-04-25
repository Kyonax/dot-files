#!/bin/bash
# Groups rules into workers -> JSON to stdout
# Usage: bash scripts/select-rules.sh < detect.json | bash scripts/worker-dispatch.sh [--min 5] [--max 25] [--max-workers 8]
#
# Reads rule file paths from stdin (one per line, relative to skill root).
# Groups by parent directory, merges small groups, splits large groups,
# and caps total workers.

set -euo pipefail

# ── resolve skill root ──────────────────────────────────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SKILL_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# ── args ─────────────────────────────────────────────────────────────────────
MIN_RULES=5
MAX_RULES=25
MAX_WORKERS=8

while [[ $# -gt 0 ]]; do
  case "$1" in
    --min)         MIN_RULES="$2"; shift 2 ;;
    --max)         MAX_RULES="$2"; shift 2 ;;
    --max-workers) MAX_WORKERS="$2"; shift 2 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

# ── read rule paths from stdin ───────────────────────────────────────────────
ALL_RULES=()
while IFS= read -r line; do
  [[ -n "$line" ]] && ALL_RULES+=("$line")
done

if [[ ${#ALL_RULES[@]} -eq 0 ]]; then
  cat <<'ENDJSON'
{
  "workers": [],
  "totalWorkers": 0,
  "totalRules": 0
}
ENDJSON
  exit 0
fi

# ── group by parent directory ────────────────────────────────────────────────
# Parent directory = the deepest directory segment before the filename.
# e.g., "universal/ada/heading-hierarchy.md" -> "universal/ada"
# e.g., "project/mr-dotcom/styling/font-utility.md" -> "project/mr-dotcom/styling"

declare -A GROUP_FILES  # group_key -> space-separated file list
declare -a GROUP_ORDER  # preserve insertion order

get_group_key() {
  local path="$1"
  local dir="${path%/*}"
  if [[ "$dir" == "$path" ]]; then
    echo "root"
  else
    echo "$dir"
  fi
}

for rule in "${ALL_RULES[@]}"; do
  key="$(get_group_key "$rule")"
  if [[ -z "${GROUP_FILES[$key]+_}" ]]; then
    GROUP_FILES["$key"]="$rule"
    GROUP_ORDER+=("$key")
  else
    GROUP_FILES["$key"]="${GROUP_FILES[$key]}"$'\n'"$rule"
  fi
done

# ── helper: count files in a group ──────────────────────────────────────────
count_group() {
  local key="$1"
  echo "${GROUP_FILES[$key]}" | grep -c '[^[:space:]]'
}

# ── merge small groups (< min) into nearest related group ────────────────────
# "Nearest related" = group sharing the longest common prefix.
# If no related group exists, merge into the smallest existing group.

common_prefix_len() {
  local a="$1" b="$2"
  local len=0 max
  max=${#a}
  [[ ${#b} -lt $max ]] && max=${#b}
  for (( i=0; i<max; i++ )); do
    if [[ "${a:$i:1}" == "${b:$i:1}" ]]; then
      ((len++))
    else
      break
    fi
  done
  echo "$len"
}

MERGED=true
while [[ "$MERGED" == true ]]; do
  MERGED=false
  NEW_ORDER=()

  for key in "${GROUP_ORDER[@]}"; do
    [[ -z "${GROUP_FILES[$key]+_}" ]] && continue
    NEW_ORDER+=("$key")
  done
  GROUP_ORDER=("${NEW_ORDER[@]+"${NEW_ORDER[@]}"}")

  for key in "${GROUP_ORDER[@]}"; do
    [[ -z "${GROUP_FILES[$key]+_}" ]] && continue
    local_count="$(count_group "$key")"
    if [[ $local_count -lt $MIN_RULES && ${#GROUP_ORDER[@]} -gt 1 ]]; then
      # Find best merge target
      BEST_TARGET=""
      BEST_PREFIX=0
      SMALLEST_TARGET=""
      SMALLEST_COUNT=999999

      for other in "${GROUP_ORDER[@]}"; do
        [[ "$other" == "$key" ]] && continue
        [[ -z "${GROUP_FILES[$other]+_}" ]] && continue

        plen="$(common_prefix_len "$key" "$other")"
        other_count="$(count_group "$other")"

        if [[ $plen -gt $BEST_PREFIX ]]; then
          BEST_PREFIX=$plen
          BEST_TARGET="$other"
        fi

        if [[ $other_count -lt $SMALLEST_COUNT ]]; then
          SMALLEST_COUNT=$other_count
          SMALLEST_TARGET="$other"
        fi
      done

      # Prefer prefix match, fall back to smallest
      TARGET="${BEST_TARGET:-$SMALLEST_TARGET}"
      if [[ -n "$TARGET" ]]; then
        GROUP_FILES["$TARGET"]="${GROUP_FILES[$TARGET]}"$'\n'"${GROUP_FILES[$key]}"
        unset "GROUP_FILES[$key]"
        MERGED=true
      fi
    fi
  done
done

# Rebuild GROUP_ORDER after merges
FINAL_ORDER=()
for key in "${GROUP_ORDER[@]}"; do
  [[ -n "${GROUP_FILES[$key]+_}" ]] && FINAL_ORDER+=("$key")
done
GROUP_ORDER=("${FINAL_ORDER[@]+"${FINAL_ORDER[@]}"}")

# ── split large groups (> max) ───────────────────────────────────────────────
declare -A SPLIT_GROUPS
declare -a SPLIT_ORDER

for key in "${GROUP_ORDER[@]}"; do
  local_count="$(count_group "$key")"
  if [[ $local_count -gt $MAX_RULES ]]; then
    # Split into chunks of MAX_RULES
    CHUNK=0
    CHUNK_COUNT=0
    CHUNK_KEY="${key}__chunk${CHUNK}"
    SPLIT_GROUPS["$CHUNK_KEY"]=""
    SPLIT_ORDER+=("$CHUNK_KEY")

    while IFS= read -r rule; do
      [[ -z "$rule" ]] && continue
      if [[ $CHUNK_COUNT -ge $MAX_RULES ]]; then
        ((CHUNK++))
        CHUNK_KEY="${key}__chunk${CHUNK}"
        SPLIT_GROUPS["$CHUNK_KEY"]=""
        SPLIT_ORDER+=("$CHUNK_KEY")
        CHUNK_COUNT=0
      fi
      if [[ -z "${SPLIT_GROUPS[$CHUNK_KEY]}" ]]; then
        SPLIT_GROUPS["$CHUNK_KEY"]="$rule"
      else
        SPLIT_GROUPS["$CHUNK_KEY"]="${SPLIT_GROUPS[$CHUNK_KEY]}"$'\n'"$rule"
      fi
      ((CHUNK_COUNT++))
    done <<< "${GROUP_FILES[$key]}"
  else
    SPLIT_GROUPS["$key"]="${GROUP_FILES[$key]}"
    SPLIT_ORDER+=("$key")
  fi
done

# ── cap total workers at max-workers (merge smallest) ────────────────────────
while [[ ${#SPLIT_ORDER[@]} -gt $MAX_WORKERS ]]; do
  # Find the two smallest groups and merge them
  SMALLEST_KEY=""
  SMALLEST_COUNT=999999
  SECOND_KEY=""
  SECOND_COUNT=999999

  for key in "${SPLIT_ORDER[@]}"; do
    [[ -z "${SPLIT_GROUPS[$key]+_}" ]] && continue
    cnt="$(echo "${SPLIT_GROUPS[$key]}" | grep -c '[^[:space:]]')"
    if [[ $cnt -lt $SMALLEST_COUNT ]]; then
      SECOND_KEY="$SMALLEST_KEY"
      SECOND_COUNT=$SMALLEST_COUNT
      SMALLEST_KEY="$key"
      SMALLEST_COUNT=$cnt
    elif [[ $cnt -lt $SECOND_COUNT ]]; then
      SECOND_KEY="$key"
      SECOND_COUNT=$cnt
    fi
  done

  if [[ -n "$SMALLEST_KEY" && -n "$SECOND_KEY" ]]; then
    SPLIT_GROUPS["$SECOND_KEY"]="${SPLIT_GROUPS[$SECOND_KEY]}"$'\n'"${SPLIT_GROUPS[$SMALLEST_KEY]}"
    unset "SPLIT_GROUPS[$SMALLEST_KEY]"
    # Rebuild order
    NEW_SPLIT_ORDER=()
    for key in "${SPLIT_ORDER[@]}"; do
      [[ -n "${SPLIT_GROUPS[$key]+_}" ]] && NEW_SPLIT_ORDER+=("$key")
    done
    SPLIT_ORDER=("${NEW_SPLIT_ORDER[@]}")
  else
    break
  fi
done

# ── build JSON output ────────────────────────────────────────────────────────
TOTAL_RULES=0
TOTAL_WORKERS=${#SPLIT_ORDER[@]}

derive_id() {
  # Derive a worker id from the group key
  # "universal/ada" -> "universal-ada"
  # "framework/vue3__chunk0" -> "framework-vue3-0"
  local key="$1"
  echo "$key" | sed 's|/|-|g; s|__chunk|-|g'
}

derive_category() {
  # Derive a human-readable category name
  # "universal/ada" -> "Universal / Ada"
  # "project/mr-dotcom/styling" -> "Project / Mr Dotcom / Styling"
  local key="$1"
  key="${key//__chunk*//}"  # strip chunk suffix
  echo "$key" | sed 's|/| / |g; s|-| |g' | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) tolower(substr($i,2))}1'
}

find_index_md() {
  # Find the INDEX.md for a group directory
  local key="$1"
  # Strip chunk suffix
  local base_key="${key//__chunk*/}"
  local index_path="$SKILL_DIR/$base_key/INDEX.md"
  if [[ -f "$index_path" ]]; then
    echo "$base_key/INDEX.md"
  else
    # Try parent directory
    local parent="${base_key%/*}"
    if [[ "$parent" != "$base_key" && -f "$SKILL_DIR/$parent/INDEX.md" ]]; then
      echo "$parent/INDEX.md"
    else
      echo "null"
    fi
  fi
}

echo "{"
echo "  \"workers\": ["

FIRST_WORKER=true
for key in "${SPLIT_ORDER[@]}"; do
  [[ -z "${SPLIT_GROUPS[$key]+_}" ]] && continue

  # Collect rules for this worker
  WORKER_RULES=()
  while IFS= read -r rule; do
    [[ -n "$rule" ]] && WORKER_RULES+=("$rule")
  done <<< "${SPLIT_GROUPS[$key]}"

  RULE_COUNT=${#WORKER_RULES[@]}
  TOTAL_RULES=$((TOTAL_RULES + RULE_COUNT))

  WORKER_ID="$(derive_id "$key")"
  CATEGORY="$(derive_category "$key")"
  INDEX_PATH="$(find_index_md "$key")"

  if [[ "$FIRST_WORKER" == true ]]; then
    FIRST_WORKER=false
  else
    echo ","
  fi

  # Build rules JSON array
  RULES_JSON="["
  FIRST_RULE=true
  for rule in "${WORKER_RULES[@]}"; do
    if [[ "$FIRST_RULE" == true ]]; then
      FIRST_RULE=false
    else
      RULES_JSON+=","
    fi
    RULES_JSON+="\"$rule\""
  done
  RULES_JSON+="]"

  # Quote index path or emit null
  if [[ "$INDEX_PATH" == "null" ]]; then
    INDEX_JSON="null"
  else
    INDEX_JSON="\"$INDEX_PATH\""
  fi

  printf '    {\n'
  printf '      "id": "%s",\n' "$WORKER_ID"
  printf '      "category": "%s",\n' "$CATEGORY"
  printf '      "indexMd": %s,\n' "$INDEX_JSON"
  printf '      "rules": %s,\n' "$RULES_JSON"
  printf '      "ruleCount": %d\n' "$RULE_COUNT"
  printf '    }'
done

echo ""
echo "  ],"
echo "  \"totalWorkers\": $TOTAL_WORKERS,"
echo "  \"totalRules\": $TOTAL_RULES"
echo "}"
