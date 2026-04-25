#!/bin/bash
# Raw YAML findings -> formatted markdown to stdout
# Usage: cat worker-output.yaml | bash scripts/format-findings.sh [--mode interactive|report]
# Or:   bash scripts/format-findings.sh < combined-findings.yaml
#
# Input format (simple YAML):
#   - rule: rule-name
#     file: path/to/file.ext
#     line: 42
#     severity: HIGH
#     problem: Description of the problem
#     before: |
#       old code snippet
#     after: |
#       new code snippet
#
# Modes:
#   interactive (default) - Per-finding output with "Implement or skip?" prompt
#   report                - Full markdown report with summary table

set -euo pipefail

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

die() {
  echo "ERROR: $*" >&2
  exit 1
}

usage() {
  cat <<'USAGE'
Usage: cat findings.yaml | bash scripts/format-findings.sh [--mode interactive|report]

Options:
  --mode interactive   Per-finding output with action prompt (default)
  --mode report        Full markdown report with summary and all findings

Input: YAML on stdin with findings (- rule: / file: / line: / severity: / problem: / before: / after:)
Output: Formatted markdown to stdout
USAGE
  exit 1
}

# severity_rank: returns a numeric sort key (lower = higher priority)
severity_rank() {
  case "$1" in
    CRITICAL) echo 0 ;;
    HIGH)     echo 1 ;;
    MEDIUM)   echo 2 ;;
    LOW)      echo 3 ;;
    *)        echo 9 ;;
  esac
}

severity_emoji() {
  case "$1" in
    CRITICAL) echo "!!!" ;;
    HIGH)     echo "!!" ;;
    MEDIUM)   echo "!" ;;
    LOW)      echo "~" ;;
    *)        echo "?" ;;
  esac
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

MODE="interactive"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode)
      [[ -z "${2:-}" ]] && die "--mode requires a value (interactive|report)"
      MODE="$2"
      if [[ "$MODE" != "interactive" && "$MODE" != "report" ]]; then
        die "Invalid mode: $MODE. Must be 'interactive' or 'report'."
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

# Check stdin is not a terminal (i.e., we have piped input)
if [[ -t 0 ]]; then
  die "No input on stdin. Pipe YAML findings into this script."
fi

# ---------------------------------------------------------------------------
# Parse YAML from stdin (line-by-line state machine)
# ---------------------------------------------------------------------------

# We store findings in parallel indexed arrays (bash 3+ compatible).
# Each finding has: rule, file, line, severity, problem, before, after

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
    # Remove trailing newline
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
  # Detect new finding: starts with "- rule:"
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

  # Skip if we haven't started any finding yet
  [[ $IDX -lt 0 ]] && continue

  # If we are inside a multiline block, check if this line is a continuation
  if $IN_MULTILINE; then
    # A new top-level field (indented key with colon) ends the multiline block
    if [[ "$line" =~ ^[[:space:]]{2,}(rule|file|line|severity|problem|before|after):[[:space:]]*(.*) ]]; then
      flush_multiline
      # Fall through to field parsing below
    else
      # Continuation line: strip up to 6 leading spaces for block indentation
      local_line="${line#      }"  # strip exactly 6 spaces
      if [[ "$local_line" == "$line" ]]; then
        # Try stripping 4 spaces
        local_line="${line#    }"
      fi
      MULTILINE_VALUE+="${local_line}"$'\n'
      continue
    fi
  fi

  # Field parsing: "  key: value" or "  key: |"
  if [[ "$line" =~ ^[[:space:]]+(file):[[:space:]]*(.*) ]]; then
    flush_multiline
    F_FILE[$IDX]="${BASH_REMATCH[2]}"
  elif [[ "$line" =~ ^[[:space:]]+(line):[[:space:]]*(.*) ]]; then
    flush_multiline
    F_LINE[$IDX]="${BASH_REMATCH[2]}"
  elif [[ "$line" =~ ^[[:space:]]+(severity):[[:space:]]*(.*) ]]; then
    flush_multiline
    F_SEVERITY[$IDX]="${BASH_REMATCH[2]}"
  elif [[ "$line" =~ ^[[:space:]]+(problem):[[:space:]]*(.*) ]]; then
    flush_multiline
    val="${BASH_REMATCH[2]}"
    if [[ "$val" == "|" || "$val" == "|-" || "$val" == "|+" ]]; then
      IN_MULTILINE=true
      CURRENT_FIELD="problem"
      MULTILINE_VALUE=""
    else
      F_PROBLEM[$IDX]="$val"
    fi
  elif [[ "$line" =~ ^[[:space:]]+(before):[[:space:]]*(.*) ]]; then
    flush_multiline
    val="${BASH_REMATCH[2]}"
    if [[ "$val" == "|" || "$val" == "|-" || "$val" == "|+" ]]; then
      IN_MULTILINE=true
      CURRENT_FIELD="before"
      MULTILINE_VALUE=""
    else
      F_BEFORE[$IDX]="$val"
    fi
  elif [[ "$line" =~ ^[[:space:]]+(after):[[:space:]]*(.*) ]]; then
    flush_multiline
    val="${BASH_REMATCH[2]}"
    if [[ "$val" == "|" || "$val" == "|-" || "$val" == "|+" ]]; then
      IN_MULTILINE=true
      CURRENT_FIELD="after"
      MULTILINE_VALUE=""
    else
      F_AFTER[$IDX]="$val"
    fi
  elif [[ "$line" =~ ^[[:space:]]+(rule):[[:space:]]*(.*) ]]; then
    # "rule:" without the leading "- " means a continuation rule field (unlikely but handle)
    flush_multiline
    F_RULE[$IDX]="${BASH_REMATCH[2]}"
  fi
done

# Flush any remaining multiline content
flush_multiline

TOTAL=$(( IDX + 1 ))

if [[ $TOTAL -eq 0 ]]; then
  die "No findings parsed from input. Check the YAML format."
fi

# ---------------------------------------------------------------------------
# Sort by severity (CRITICAL > HIGH > MEDIUM > LOW)
# Build an index array sorted by severity rank, then by file+line
# ---------------------------------------------------------------------------

# Create sortable lines: "rank|file|line|original_index"
SORT_LINES=""
for (( i = 0; i < TOTAL; i++ )); do
  rank=$(severity_rank "${F_SEVERITY[$i]}")
  SORT_LINES+="${rank}|${F_FILE[$i]}|${F_LINE[$i]}|${i}"$'\n'
done

# Sort numerically by rank, then by file, then by line
SORTED_INDICES=()
while IFS='|' read -r _rank _file _line orig_idx; do
  [[ -z "$orig_idx" ]] && continue
  SORTED_INDICES+=("$orig_idx")
done < <(echo "$SORT_LINES" | sort -t'|' -k1,1n -k2,2 -k3,3n)

# ---------------------------------------------------------------------------
# Deduplicate: same file + line -> keep highest severity (first in sorted order)
# ---------------------------------------------------------------------------

declare -A SEEN_FILE_LINE=()
DEDUPED_INDICES=()

for idx in "${SORTED_INDICES[@]}"; do
  key="${F_FILE[$idx]}:${F_LINE[$idx]}"
  if [[ -z "${SEEN_FILE_LINE[$key]:-}" ]]; then
    SEEN_FILE_LINE[$key]=1
    DEDUPED_INDICES+=("$idx")
  fi
done

DEDUPED_TOTAL=${#DEDUPED_INDICES[@]}

# ---------------------------------------------------------------------------
# Check pre-existing: if a diff file exists, mark findings not in the diff
# ---------------------------------------------------------------------------

# Look for the most recent diff file in /tmp/cr-pr-diff/
DIFF_FILE=""
if [[ -d /tmp/cr-pr-diff ]]; then
  DIFF_FILE=$(ls -t /tmp/cr-pr-diff/*.diff 2>/dev/null | head -1 || true)
fi

# Build a set of changed lines from the diff (file:line format)
declare -A CHANGED_LINES=()
if [[ -n "$DIFF_FILE" && -f "$DIFF_FILE" ]]; then
  CURRENT_DIFF_FILE=""
  CURRENT_LINE_NUM=0
  while IFS= read -r dline; do
    # Track current file
    if [[ "$dline" =~ ^\+\+\+[[:space:]]b/(.*) ]]; then
      CURRENT_DIFF_FILE="${BASH_REMATCH[1]}"
      continue
    fi
    # Track hunk header: @@ -old,count +new,count @@
    if [[ "$dline" =~ ^@@[[:space:]]-[0-9]+(,[0-9]+)?[[:space:]]\+([0-9]+)(,[0-9]+)?[[:space:]]@@ ]]; then
      CURRENT_LINE_NUM="${BASH_REMATCH[2]}"
      continue
    fi
    # Count lines in the new file
    if [[ -n "$CURRENT_DIFF_FILE" && $CURRENT_LINE_NUM -gt 0 ]]; then
      if [[ "$dline" =~ ^\+ ]]; then
        # Added/changed line
        CHANGED_LINES["${CURRENT_DIFF_FILE}:${CURRENT_LINE_NUM}"]=1
        CURRENT_LINE_NUM=$(( CURRENT_LINE_NUM + 1 ))
      elif [[ "$dline" =~ ^- ]]; then
        # Removed line (does not advance new-file line counter)
        :
      else
        # Context line
        CURRENT_LINE_NUM=$(( CURRENT_LINE_NUM + 1 ))
      fi
    fi
  done < "$DIFF_FILE"
fi

# Mark pre-existing findings
declare -a F_PREEXISTING=()
for idx in "${DEDUPED_INDICES[@]}"; do
  key="${F_FILE[$idx]}:${F_LINE[$idx]}"
  if [[ -n "$DIFF_FILE" && ${#CHANGED_LINES[@]} -gt 0 ]]; then
    if [[ -z "${CHANGED_LINES[$key]:-}" ]]; then
      F_PREEXISTING[$idx]="true"
    else
      F_PREEXISTING[$idx]="false"
    fi
  else
    F_PREEXISTING[$idx]="false"
  fi
done

# ---------------------------------------------------------------------------
# Output: interactive mode
# ---------------------------------------------------------------------------

if [[ "$MODE" == "interactive" ]]; then
  FINDING_NUM=0
  for idx in "${DEDUPED_INDICES[@]}"; do
    FINDING_NUM=$(( FINDING_NUM + 1 ))

    sev="${F_SEVERITY[$idx]}"
    sev_marker=$(severity_emoji "$sev")
    preexisting="${F_PREEXISTING[$idx]:-false}"

    echo "---"
    echo ""
    echo "### Finding ${FINDING_NUM}/${DEDUPED_TOTAL}  [${sev}] ${sev_marker}"
    echo ""
    echo "**Rule:** ${F_RULE[$idx]}"
    echo "**File:** \`${F_FILE[$idx]}\` (line ${F_LINE[$idx]})"
    echo "**Severity:** ${sev}"

    if [[ "$preexisting" == "true" ]]; then
      echo "**Status:** Pre-existing (skip recommended)"
    fi

    echo ""
    echo "**Problem:** ${F_PROBLEM[$idx]}"

    if [[ -n "${F_BEFORE[$idx]}" ]]; then
      echo ""
      echo "**Before:**"
      echo '```'
      echo "${F_BEFORE[$idx]}"
      echo '```'
    fi

    if [[ -n "${F_AFTER[$idx]}" ]]; then
      echo ""
      echo "**After:**"
      echo '```'
      echo "${F_AFTER[$idx]}"
      echo '```'
    fi

    echo ""
    if [[ "$preexisting" == "true" ]]; then
      echo "> Implement, skip, or defer? (pre-existing issue, skip recommended)"
    else
      echo "> Implement or skip?"
    fi
    echo ""
  done
  exit 0
fi

# ---------------------------------------------------------------------------
# Output: report mode
# ---------------------------------------------------------------------------

if [[ "$MODE" == "report" ]]; then

  # Count by severity
  CRITICAL_COUNT=0
  HIGH_COUNT=0
  MEDIUM_COUNT=0
  LOW_COUNT=0
  PREEXISTING_COUNT=0

  for idx in "${DEDUPED_INDICES[@]}"; do
    case "${F_SEVERITY[$idx]}" in
      CRITICAL) CRITICAL_COUNT=$(( CRITICAL_COUNT + 1 )) ;;
      HIGH)     HIGH_COUNT=$(( HIGH_COUNT + 1 )) ;;
      MEDIUM)   MEDIUM_COUNT=$(( MEDIUM_COUNT + 1 )) ;;
      LOW)      LOW_COUNT=$(( LOW_COUNT + 1 )) ;;
    esac
    if [[ "${F_PREEXISTING[$idx]:-false}" == "true" ]]; then
      PREEXISTING_COUNT=$(( PREEXISTING_COUNT + 1 ))
    fi
  done

  echo "# Code Review Report"
  echo ""
  echo "## Summary"
  echo ""
  echo "| Metric | Count |"
  echo "|--------|-------|"
  echo "| Total findings | ${DEDUPED_TOTAL} |"
  echo "| Critical | ${CRITICAL_COUNT} |"
  echo "| High | ${HIGH_COUNT} |"
  echo "| Medium | ${MEDIUM_COUNT} |"
  echo "| Low | ${LOW_COUNT} |"
  echo "| Pre-existing (skip recommended) | ${PREEXISTING_COUNT} |"
  echo ""

  echo "## Severity Breakdown"
  echo ""

  if [[ $CRITICAL_COUNT -gt 0 ]]; then
    echo "- **CRITICAL (${CRITICAL_COUNT}):** Must fix before merge"
  fi
  if [[ $HIGH_COUNT -gt 0 ]]; then
    echo "- **HIGH (${HIGH_COUNT}):** Should fix before merge"
  fi
  if [[ $MEDIUM_COUNT -gt 0 ]]; then
    echo "- **MEDIUM (${MEDIUM_COUNT}):** Recommended improvements"
  fi
  if [[ $LOW_COUNT -gt 0 ]]; then
    echo "- **LOW (${LOW_COUNT}):** Minor / stylistic"
  fi
  echo ""

  echo "## Findings"
  echo ""

  FINDING_NUM=0
  for idx in "${DEDUPED_INDICES[@]}"; do
    FINDING_NUM=$(( FINDING_NUM + 1 ))

    sev="${F_SEVERITY[$idx]}"
    sev_marker=$(severity_emoji "$sev")
    preexisting="${F_PREEXISTING[$idx]:-false}"

    echo "### ${FINDING_NUM}. [${sev}] ${F_RULE[$idx]}"
    echo ""
    echo "- **File:** \`${F_FILE[$idx]}\` (line ${F_LINE[$idx]})"
    echo "- **Severity:** ${sev} ${sev_marker}"

    if [[ "$preexisting" == "true" ]]; then
      echo "- **Status:** Pre-existing (skip recommended)"
    fi

    echo ""
    echo "${F_PROBLEM[$idx]}"

    if [[ -n "${F_BEFORE[$idx]}" ]]; then
      echo ""
      echo "**Before:**"
      echo '```'
      echo "${F_BEFORE[$idx]}"
      echo '```'
    fi

    if [[ -n "${F_AFTER[$idx]}" ]]; then
      echo ""
      echo "**After:**"
      echo '```'
      echo "${F_AFTER[$idx]}"
      echo '```'
    fi

    echo ""
  done

  echo "---"
  echo "*Report generated by code-review skill*"
  exit 0
fi
