#!/bin/bash
# ESLint on changed files -> JSON to stdout
# Usage: bash scripts/lint-changed.sh [--format json]

set -uo pipefail

FORMAT="plain"
BASE="master"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --format) FORMAT="$2"; shift 2 ;;
    --base) BASE="$2"; shift 2 ;;
    *) echo "Unknown arg: $1" >&2; exit 1 ;;
  esac
done

# Escape a string for safe JSON embedding
json_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="$(printf '%s' "$s" | sed ':a;N;$!ba;s/\n/\\n/g')"
  s="$(printf '%s' "$s" | sed 's/\t/\\t/g')"
  printf '%s' "$s"
}

# Get changed files filtered to lintable extensions
declare -a CHANGED_FILES=()
while IFS= read -r f; do
  [[ -z "$f" ]] && continue
  case "$f" in
    *.js|*.vue|*.ts|*.tsx|*.jsx)
      # Only include files that still exist (not deleted)
      if [[ -f "$f" ]]; then
        CHANGED_FILES+=("$f")
      fi
      ;;
  esac
done < <(git diff --name-only "${BASE}...HEAD" 2>/dev/null || git diff --name-only "${BASE}" HEAD 2>/dev/null || git diff --name-only HEAD 2>/dev/null)

# If no lintable files found
if [[ ${#CHANGED_FILES[@]} -eq 0 ]]; then
  if [[ "$FORMAT" == "json" ]]; then
    echo '{"passed": true, "errors": 0, "warnings": 0, "findings": [], "message": "No lintable files changed"}'
  else
    echo "No lintable files changed (.js, .vue, .ts, .tsx)"
  fi
  exit 0
fi

if [[ "$FORMAT" == "json" ]]; then
  # Run eslint with JSON output
  eslint_output="$(npx eslint "${CHANGED_FILES[@]}" --format json 2>/dev/null || true)"

  # Parse the eslint JSON into our simplified structure
  # eslint JSON is an array of file results, each with messages array
  if [[ -z "$eslint_output" || "$eslint_output" == "[]" ]]; then
    echo '{"passed": true, "errors": 0, "warnings": 0, "findings": []}'
    exit 0
  fi

  # Count errors and warnings, extract findings
  error_count=0
  warning_count=0
  findings=""
  findings_first=true

  # Use a temp file to process the JSON line by line safely
  tmpfile="$(mktemp)"
  echo "$eslint_output" > "$tmpfile"

  # Parse with sed/grep if no jq; attempt jq first
  if command -v jq &>/dev/null; then
    error_count="$(jq '[.[].messages[] | select(.severity == 2)] | length' "$tmpfile" 2>/dev/null || echo 0)"
    warning_count="$(jq '[.[].messages[] | select(.severity == 1)] | length' "$tmpfile" 2>/dev/null || echo 0)"

    findings="$(jq -r '
      [.[] | .filePath as $file |
        .messages[] |
        {
          file: $file,
          line: .line,
          rule: (.ruleId // "unknown"),
          severity: (if .severity == 2 then "error" else "warning" end),
          message: .message
        }
      ]' "$tmpfile" 2>/dev/null || echo "[]")"

    passed="true"
    if [[ "$error_count" -gt 0 ]]; then
      passed="false"
    fi

    cat <<EOF
{
  "passed": ${passed},
  "errors": ${error_count},
  "warnings": ${warning_count},
  "findings": ${findings}
}
EOF
  else
    # Fallback: output raw eslint JSON wrapped in our structure
    # Count by grep approximation
    error_count="$(grep -c '"severity":2' "$tmpfile" 2>/dev/null || echo 0)"
    warning_count="$(grep -c '"severity":1' "$tmpfile" 2>/dev/null || echo 0)"

    passed="true"
    if [[ "$error_count" -gt 0 ]]; then
      passed="false"
    fi

    cat <<EOF
{
  "passed": ${passed},
  "errors": ${error_count},
  "warnings": ${warning_count},
  "rawEslintOutput": $(cat "$tmpfile")
}
EOF
  fi

  rm -f "$tmpfile"
else
  # Plain text output
  npx eslint "${CHANGED_FILES[@]}" 2>&1 || true
fi

exit 0
