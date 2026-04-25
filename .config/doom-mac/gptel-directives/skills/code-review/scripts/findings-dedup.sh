#!/bin/bash
# De-duplicate worker findings cross-worker + against PR review digest -> YAML to stdout
# Usage: cat worker-*.yaml | bash scripts/findings-dedup.sh [--digest digest.json] [--mode strict|lenient]
#
# Cross-worker dedup: same (file, line) -> keep highest severity, merge problems.
# Digest filtering: resolved items removed (strict) or downgraded to LOW (lenient).
#
# Input: YAML findings on stdin (same format as format-findings.sh input)
# Output: De-duplicated YAML to stdout (pipe to format-findings.sh)

set -euo pipefail

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

die() {
  echo "ERROR: $*" >&2
  exit 1
}

severity_rank() {
  case "$1" in
    CRITICAL) echo 0 ;;
    HIGH)     echo 1 ;;
    MEDIUM)   echo 2 ;;
    LOW)      echo 3 ;;
    *)        echo 9 ;;
  esac
}

usage() {
  cat <<'USAGE'
Usage: cat findings.yaml | bash scripts/findings-dedup.sh [options]

Options:
  --digest <path>     PR review digest JSON from pr-review-digest.sh
  --mode strict|lenient  strict: remove resolved findings; lenient: downgrade to LOW (default: lenient)
  --help              Show this help

Input:  YAML findings on stdin
Output: De-duplicated YAML to stdout
USAGE
  exit 1
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

DIGEST_JSON=""
MODE="lenient"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --digest)
      [[ -z "${2:-}" ]] && die "--digest requires a file path"
      DIGEST_JSON="$2"
      shift 2
      ;;
    --mode)
      [[ -z "${2:-}" ]] && die "--mode requires a value (strict|lenient)"
      MODE="$2"
      if [[ "$MODE" != "strict" && "$MODE" != "lenient" ]]; then
        die "Invalid mode: $MODE. Must be 'strict' or 'lenient'."
      fi
      shift 2
      ;;
    --help|-h)
      usage
      ;;
    *)
      die "Unexpected argument: $1"
      ;;
  esac
done

if [[ -t 0 ]]; then
  die "No input on stdin. Pipe YAML findings into this script."
fi

# ---------------------------------------------------------------------------
# Parse YAML from stdin (same state machine as format-findings.sh)
# ---------------------------------------------------------------------------

declare -a F_RULE=()
declare -a F_FILE=()
declare -a F_LINE=()
declare -a F_SEVERITY=()
declare -a F_PROBLEM=()
declare -a F_BEFORE=()
declare -a F_AFTER=()

IDX=-1
CURRENT_FIELD=""
MULTILINE_VALUE=""
IN_MULTILINE=false

flush_multiline() {
  if $IN_MULTILINE && [[ $IDX -ge 0 ]]; then
    MULTILINE_VALUE="${MULTILINE_VALUE%$'\n'}"
    case "$CURRENT_FIELD" in
      problem) F_PROBLEM[$IDX]="$MULTILINE_VALUE" ;;
      before)  F_BEFORE[$IDX]="$MULTILINE_VALUE" ;;
      after)   F_AFTER[$IDX]="$MULTILINE_VALUE" ;;
    esac
  fi
  IN_MULTILINE=false
  MULTILINE_VALUE=""
  CURRENT_FIELD=""
}

while IFS= read -r line || [[ -n "$line" ]]; do
  # New finding
  if [[ "$line" =~ ^[[:space:]]*-[[:space:]]+rule:[[:space:]]*(.*) ]]; then
    flush_multiline
    IDX=$(( IDX + 1 ))
    F_RULE[$IDX]="${BASH_REMATCH[1]}"
    F_FILE[$IDX]=""
    F_LINE[$IDX]="0"
    F_SEVERITY[$IDX]="MEDIUM"
    F_PROBLEM[$IDX]=""
    F_BEFORE[$IDX]=""
    F_AFTER[$IDX]=""
    CURRENT_FIELD=""
    continue
  fi

  [[ $IDX -lt 0 ]] && continue

  # Multiline continuation
  if $IN_MULTILINE; then
    if [[ "$line" =~ ^[[:space:]]{2,}(rule|file|line|severity|problem|before|after):[[:space:]]*(.*) ]]; then
      flush_multiline
    else
      local_line="${line#      }"
      if [[ "$local_line" == "$line" ]]; then
        local_line="${line#    }"
      fi
      MULTILINE_VALUE+="${local_line}"$'\n'
      continue
    fi
  fi

  # Field parsing
  if [[ "$line" =~ ^[[:space:]]+(file):[[:space:]]*(.*) ]]; then
    F_FILE[$IDX]="${BASH_REMATCH[2]}"
  elif [[ "$line" =~ ^[[:space:]]+(line):[[:space:]]*(.*) ]]; then
    F_LINE[$IDX]="${BASH_REMATCH[2]}"
  elif [[ "$line" =~ ^[[:space:]]+(severity):[[:space:]]*(.*) ]]; then
    F_SEVERITY[$IDX]="${BASH_REMATCH[2]}"
  elif [[ "$line" =~ ^[[:space:]]+(problem):[[:space:]]*(.*) ]]; then
    val="${BASH_REMATCH[2]}"
    if [[ "$val" == "|" || "$val" == ">" ]]; then
      IN_MULTILINE=true
      CURRENT_FIELD="problem"
      MULTILINE_VALUE=""
    else
      F_PROBLEM[$IDX]="$val"
    fi
  elif [[ "$line" =~ ^[[:space:]]+(before):[[:space:]]*(.*) ]]; then
    val="${BASH_REMATCH[2]}"
    if [[ "$val" == "|" || "$val" == ">" || -z "$val" ]]; then
      IN_MULTILINE=true
      CURRENT_FIELD="before"
      MULTILINE_VALUE=""
    else
      F_BEFORE[$IDX]="$val"
    fi
  elif [[ "$line" =~ ^[[:space:]]+(after):[[:space:]]*(.*) ]]; then
    val="${BASH_REMATCH[2]}"
    if [[ "$val" == "|" || "$val" == ">" || -z "$val" ]]; then
      IN_MULTILINE=true
      CURRENT_FIELD="after"
      MULTILINE_VALUE=""
    else
      F_AFTER[$IDX]="$val"
    fi
  fi
done

flush_multiline

TOTAL=$(( IDX + 1 ))

if [[ $TOTAL -eq 0 ]]; then
  echo "# NO FINDINGS"
  exit 0
fi

# ---------------------------------------------------------------------------
# Load digest for filtering
# ---------------------------------------------------------------------------

declare -A DIGEST_RESOLVED=()
declare -A DIGEST_PENDING=()

if [[ -n "$DIGEST_JSON" ]] && [[ -f "$DIGEST_JSON" ]]; then
  # Extract resolved (file:line) pairs from digest
  while IFS= read -r entry; do
    if [[ -n "$entry" ]]; then
      DIGEST_RESOLVED["$entry"]=1
    fi
  done < <(python3 -c "
import json, sys, os
with open('$DIGEST_JSON') as f:
    data = json.load(f)
for item in data.get('digest', {}).get('resolved', []):
    basename = os.path.basename(item.get('path', ''))
    line = item.get('line', 0)
    # Output with fuzzy range: line-3 to line+3
    for offset in range(-3, 4):
        print(f'{basename}:{line + offset}')
" 2>/dev/null)

  while IFS= read -r entry; do
    if [[ -n "$entry" ]]; then
      DIGEST_PENDING["$entry"]=1
    fi
  done < <(python3 -c "
import json, sys, os
with open('$DIGEST_JSON') as f:
    data = json.load(f)
for item in data.get('digest', {}).get('pending', []):
    basename = os.path.basename(item.get('path', ''))
    line = item.get('line', 0)
    for offset in range(-3, 4):
        print(f'{basename}:{line + offset}')
" 2>/dev/null)
fi

# ---------------------------------------------------------------------------
# Cross-worker dedup: group by (file, line), keep highest severity
# ---------------------------------------------------------------------------

declare -A SEEN_KEY=()     # key -> best index
declare -a KEEP_IDX=()     # indices to keep (in order)
declare -a SKIP_IDX=()     # indices to skip (merged into another)

for (( i = 0; i < TOTAL; i++ )); do
  file_basename=$(basename "${F_FILE[$i]}")
  key="${file_basename}:${F_LINE[$i]}"

  if [[ -n "${SEEN_KEY[$key]:-}" ]]; then
    # Duplicate — compare severity
    existing_idx="${SEEN_KEY[$key]}"
    existing_rank=$(severity_rank "${F_SEVERITY[$existing_idx]}")
    new_rank=$(severity_rank "${F_SEVERITY[$i]}")

    if [[ $new_rank -lt $existing_rank ]]; then
      # New finding has higher severity — replace
      SKIP_IDX+=("$existing_idx")
      SEEN_KEY["$key"]=$i
      # Merge problem descriptions
      F_PROBLEM[$i]="${F_PROBLEM[$i]} [Also: ${F_RULE[$existing_idx]}: ${F_PROBLEM[$existing_idx]}]"
      # Replace in KEEP_IDX
      for (( k = 0; k < ${#KEEP_IDX[@]}; k++ )); do
        if [[ "${KEEP_IDX[$k]}" == "$existing_idx" ]]; then
          KEEP_IDX[$k]=$i
          break
        fi
      done
    else
      # Existing has higher or equal severity — skip new
      SKIP_IDX+=("$i")
      F_PROBLEM[$existing_idx]="${F_PROBLEM[$existing_idx]} [Also: ${F_RULE[$i]}: ${F_PROBLEM[$i]}]"
    fi
  else
    SEEN_KEY["$key"]=$i
    KEEP_IDX+=("$i")
  fi
done

# ---------------------------------------------------------------------------
# Digest filtering
# ---------------------------------------------------------------------------

declare -a FINAL_IDX=()

for idx in "${KEEP_IDX[@]}"; do
  # Check if in skip list
  is_skipped=false
  for s in "${SKIP_IDX[@]}"; do
    if [[ "$s" == "$idx" ]]; then
      is_skipped=true
      break
    fi
  done
  $is_skipped && continue

  file_basename=$(basename "${F_FILE[$idx]}")
  key="${file_basename}:${F_LINE[$idx]}"

  # Check against digest
  if [[ -n "${DIGEST_RESOLVED[$key]:-}" ]]; then
    if [[ "$MODE" == "strict" ]]; then
      continue  # Remove entirely
    else
      # Lenient: downgrade to LOW
      F_SEVERITY[$idx]="LOW"
      F_PROBLEM[$idx]="[Previously flagged and resolved] ${F_PROBLEM[$idx]}"
    fi
  fi

  if [[ -n "${DIGEST_PENDING[$key]:-}" ]]; then
    F_PROBLEM[$idx]="${F_PROBLEM[$idx]} [Note: also flagged by bot reviewer, pending response]"
  fi

  FINAL_IDX+=("$idx")
done

# ---------------------------------------------------------------------------
# Output de-duplicated YAML
# ---------------------------------------------------------------------------

if [[ ${#FINAL_IDX[@]} -eq 0 ]]; then
  echo "# NO FINDINGS (all filtered)"
  exit 0
fi

for idx in "${FINAL_IDX[@]}"; do
  echo "- rule: ${F_RULE[$idx]}"
  echo "  file: ${F_FILE[$idx]}"
  echo "  line: ${F_LINE[$idx]}"
  echo "  severity: ${F_SEVERITY[$idx]}"

  # Problem
  if [[ "${F_PROBLEM[$idx]}" == *$'\n'* ]]; then
    echo "  problem: |"
    while IFS= read -r pline; do
      echo "    $pline"
    done <<< "${F_PROBLEM[$idx]}"
  else
    echo "  problem: ${F_PROBLEM[$idx]}"
  fi

  # Before
  if [[ -n "${F_BEFORE[$idx]}" ]]; then
    echo "  before: |"
    while IFS= read -r bline; do
      echo "    $bline"
    done <<< "${F_BEFORE[$idx]}"
  fi

  # After
  if [[ -n "${F_AFTER[$idx]}" ]]; then
    echo "  after: |"
    while IFS= read -r aline; do
      echo "    $aline"
    done <<< "${F_AFTER[$idx]}"
  fi

  echo ""
done
