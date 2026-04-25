#!/bin/bash
# Test runner on changed files -> JSON to stdout
# Usage: bash scripts/test-changed.sh [--framework vitest|jest]

set -uo pipefail

FRAMEWORK=""
BASE="master"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --framework) FRAMEWORK="$2"; shift 2 ;;
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

# Auto-detect test framework if not specified
detect_framework() {
  if [[ -n "$FRAMEWORK" ]]; then
    echo "$FRAMEWORK"
    return
  fi

  # Check root package.json and any nested ones
  for pkg in package.json website/package.json; do
    if [[ -f "$pkg" ]]; then
      if grep -q '"vitest"' "$pkg" 2>/dev/null; then
        echo "vitest"
        return
      fi
      if grep -q '"jest"' "$pkg" 2>/dev/null; then
        echo "jest"
        return
      fi
      if grep -q '"mocha"' "$pkg" 2>/dev/null; then
        echo "mocha"
        return
      fi
    fi
  done

  # Fallback: check for config files
  if [[ -f "vitest.config.js" || -f "vitest.config.ts" || -f "vitest.config.mjs" ]]; then
    echo "vitest"
  elif [[ -f "jest.config.js" || -f "jest.config.ts" || -f "jest.config.mjs" ]]; then
    echo "jest"
  elif [[ -f ".mocharc.yml" || -f ".mocharc.json" ]]; then
    echo "mocha"
  else
    echo "unknown"
  fi
}

# Get changed source files (non-test files)
declare -a CHANGED_SOURCES=()
while IFS= read -r f; do
  [[ -z "$f" ]] && continue
  # Skip test files themselves, only track source files
  case "$f" in
    *.test.*|*.spec.*|*__tests__/*) continue ;;
    *.js|*.ts|*.vue|*.tsx|*.jsx)
      CHANGED_SOURCES+=("$f")
      ;;
  esac
done < <(git diff --name-only "${BASE}...HEAD" 2>/dev/null || git diff --name-only "${BASE}" HEAD 2>/dev/null || git diff --name-only HEAD 2>/dev/null)

# Also include any directly changed test files
declare -a CHANGED_TESTS=()
while IFS= read -r f; do
  [[ -z "$f" ]] && continue
  case "$f" in
    *.test.*|*.spec.*|*__tests__/*)
      if [[ -f "$f" ]]; then
        CHANGED_TESTS+=("$f")
      fi
      ;;
  esac
done < <(git diff --name-only "${BASE}...HEAD" 2>/dev/null || git diff --name-only "${BASE}" HEAD 2>/dev/null || git diff --name-only HEAD 2>/dev/null)

# Find test files matching changed source files
declare -a TEST_FILES=()

for src in "${CHANGED_SOURCES[@]+"${CHANGED_SOURCES[@]}"}"; do
  # Get base name without extension
  dir="$(dirname "$src")"
  base="$(basename "$src")"
  name="${base%.*}"
  ext="${base##*.}"

  # Check common test file patterns
  candidates=(
    "${dir}/${name}.test.${ext}"
    "${dir}/${name}.spec.${ext}"
    "${dir}/${name}.test.js"
    "${dir}/${name}.spec.js"
    "${dir}/${name}.test.ts"
    "${dir}/${name}.spec.ts"
    "${dir}/__tests__/${name}.${ext}"
    "${dir}/__tests__/${name}.test.${ext}"
    "${dir}/__tests__/${name}.test.js"
    "${dir}/__tests__/${base}"
  )

  for candidate in "${candidates[@]}"; do
    if [[ -f "$candidate" ]]; then
      # Avoid duplicates
      dupe=false
      for existing in "${TEST_FILES[@]+"${TEST_FILES[@]}"}"; do
        [[ "$existing" == "$candidate" ]] && dupe=true && break
      done
      [[ "$dupe" == "false" ]] && TEST_FILES+=("$candidate")
    fi
  done
done

# Add directly changed test files
for t in "${CHANGED_TESTS[@]+"${CHANGED_TESTS[@]}"}"; do
  dupe=false
  for existing in "${TEST_FILES[@]+"${TEST_FILES[@]}"}"; do
    [[ "$existing" == "$t" ]] && dupe=true && break
  done
  [[ "$dupe" == "false" ]] && TEST_FILES+=("$t")
done

# If no test files found
if [[ ${#TEST_FILES[@]} -eq 0 ]]; then
  echo '{"passed": true, "total": 0, "passedCount": 0, "failedCount": 0, "files": [], "message": "No test files found for changed sources"}'
  exit 0
fi

# Detect framework
fw="$(detect_framework)"

# Run tests and capture output
tmpout="$(mktemp)"
tmperr="$(mktemp)"
exit_code=0

case "$fw" in
  vitest)
    npx vitest run "${TEST_FILES[@]}" --reporter=json 2>"$tmperr" >"$tmpout" || exit_code=$?
    ;;
  jest)
    npx jest "${TEST_FILES[@]}" --json --outputFile="$tmpout" 2>"$tmperr" || exit_code=$?
    ;;
  mocha)
    npx mocha "${TEST_FILES[@]}" --reporter json 2>"$tmperr" >"$tmpout" || exit_code=$?
    ;;
  *)
    echo '{"passed": false, "total": 0, "passedCount": 0, "failedCount": 0, "files": [], "message": "Could not detect test framework. Use --framework flag."}'
    rm -f "$tmpout" "$tmperr"
    exit 0
    ;;
esac

raw_output="$(cat "$tmpout")"
raw_stderr="$(cat "$tmperr")"

# Parse JSON output from test runner
if command -v jq &>/dev/null; then
  case "$fw" in
    vitest)
      # Vitest JSON reporter outputs { testResults: [...] }
      total="$(echo "$raw_output" | jq '.numTotalTests // 0' 2>/dev/null || echo 0)"
      passed_count="$(echo "$raw_output" | jq '.numPassedTests // 0' 2>/dev/null || echo 0)"
      failed_count="$(echo "$raw_output" | jq '.numFailedTests // 0' 2>/dev/null || echo 0)"
      passed_bool="true"
      [[ "$failed_count" -gt 0 ]] && passed_bool="false"

      files_json="$(echo "$raw_output" | jq '[
        .testResults[]? | {
          file: .name,
          total: (.assertionResults | length),
          passed: ([.assertionResults[]? | select(.status == "passed")] | length),
          failed: ([.assertionResults[]? | select(.status == "failed")] | length)
        }
      ]' 2>/dev/null || echo "[]")"
      ;;
    jest)
      total="$(echo "$raw_output" | jq '.numTotalTests // 0' 2>/dev/null || echo 0)"
      passed_count="$(echo "$raw_output" | jq '.numPassedTests // 0' 2>/dev/null || echo 0)"
      failed_count="$(echo "$raw_output" | jq '.numFailedTests // 0' 2>/dev/null || echo 0)"
      passed_bool="true"
      [[ "$failed_count" -gt 0 ]] && passed_bool="false"

      files_json="$(echo "$raw_output" | jq '[
        .testResults[]? | {
          file: .name,
          total: (.assertionResults | length),
          passed: ([.assertionResults[]? | select(.status == "passed")] | length),
          failed: ([.assertionResults[]? | select(.status == "failed")] | length)
        }
      ]' 2>/dev/null || echo "[]")"
      ;;
    mocha)
      total="$(echo "$raw_output" | jq '(.passes | length) + (.failures | length)' 2>/dev/null || echo 0)"
      passed_count="$(echo "$raw_output" | jq '.passes | length' 2>/dev/null || echo 0)"
      failed_count="$(echo "$raw_output" | jq '.failures | length' 2>/dev/null || echo 0)"
      passed_bool="true"
      [[ "$failed_count" -gt 0 ]] && passed_bool="false"

      files_json="[]"
      ;;
  esac

  cat <<EOF
{
  "passed": ${passed_bool},
  "total": ${total},
  "passedCount": ${passed_count},
  "failedCount": ${failed_count},
  "framework": "${fw}",
  "files": ${files_json}
}
EOF
else
  # No jq: output simplified result based on exit code
  passed_bool="true"
  [[ "$exit_code" -ne 0 ]] && passed_bool="false"

  files_list=""
  first=true
  for tf in "${TEST_FILES[@]}"; do
    [[ "$first" == "true" ]] && first=false || files_list+=","
    files_list+=" \"$(json_escape "$tf")\""
  done

  cat <<EOF
{
  "passed": ${passed_bool},
  "total": -1,
  "passedCount": -1,
  "failedCount": -1,
  "framework": "${fw}",
  "files": [${files_list} ],
  "rawOutput": "$(json_escape "$raw_output")",
  "rawStderr": "$(json_escape "$raw_stderr")",
  "message": "jq not available; raw output included"
}
EOF
fi

rm -f "$tmpout" "$tmperr"
exit 0
