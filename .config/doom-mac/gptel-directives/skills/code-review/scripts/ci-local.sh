#!/bin/bash
# Discover + run CI workflows locally -> JSON to stdout
# Usage: bash scripts/ci-local.sh [--repo-root /path] [--workflow <name>]

set -uo pipefail

REPO_ROOT="."
WORKFLOW_FILTER=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo-root) REPO_ROOT="$2"; shift 2 ;;
    --workflow) WORKFLOW_FILTER="$2"; shift 2 ;;
    *) echo "Unknown arg: $1" >&2; exit 1 ;;
  esac
done

WORKFLOWS_DIR="${REPO_ROOT}/.github/workflows"

# Escape a string for safe JSON embedding
json_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="$(printf '%s' "$s" | sed ':a;N;$!ba;s/\n/\\n/g')"
  s="$(printf '%s' "$s" | sed 's/\t/\\t/g')"
  printf '%s' "$s"
}

# Check if workflows directory exists
if [[ ! -d "$WORKFLOWS_DIR" ]]; then
  echo '{"workflowsFound": 0, "results": [], "overallStatus": "skipped", "actionRequired": [], "message": "No .github/workflows/ directory found"}'
  exit 0
fi

# Discover workflow files
declare -a WORKFLOW_FILES=()
for ymlfile in "${WORKFLOWS_DIR}"/*.yml "${WORKFLOWS_DIR}"/*.yaml; do
  [[ -f "$ymlfile" ]] || continue
  WORKFLOW_FILES+=("$ymlfile")
done

if [[ ${#WORKFLOW_FILES[@]} -eq 0 ]]; then
  echo '{"workflowsFound": 0, "results": [], "overallStatus": "skipped", "actionRequired": [], "message": "No workflow files found"}'
  exit 0
fi

# Extract run commands from a workflow YAML file using grep/sed
# Returns lines of runnable commands (npm run, npx, node)
extract_run_commands() {
  local ymlfile="$1"
  local in_run=false
  local multiline=false
  local commands=()

  while IFS= read -r line; do
    # Detect `run:` lines
    if echo "$line" | grep -qE '^\s*-?\s*run:\s*\|'; then
      # Multiline run block
      multiline=true
      continue
    elif echo "$line" | grep -qE '^\s*-?\s*run:\s*$'; then
      # Empty run: (next lines are the command)
      multiline=true
      continue
    elif echo "$line" | grep -qE '^\s*-?\s*run:\s*.+'; then
      # Single-line run command
      cmd="$(echo "$line" | sed 's/.*run:[[:space:]]*//')"
      commands+=("$cmd")
      multiline=false
      continue
    fi

    # If in a multiline block, collect indented lines
    if [[ "$multiline" == "true" ]]; then
      if echo "$line" | grep -qE '^\s{6,}' && ! echo "$line" | grep -qE '^\s*-\s' && ! echo "$line" | grep -qE '^\s*[a-zA-Z_]+:'; then
        cmd="$(echo "$line" | sed 's/^[[:space:]]*//')"
        [[ -n "$cmd" ]] && commands+=("$cmd")
      else
        multiline=false
      fi
    fi
  done < "$ymlfile"

  printf '%s\n' "${commands[@]+"${commands[@]}"}"
}

# Filter to locally-runnable commands
is_local_runnable() {
  local cmd="$1"
  # Skip commands with secrets or cloud-only patterns
  if echo "$cmd" | grep -qE '\$\{\{[[:space:]]*secrets\.' 2>/dev/null; then
    return 1
  fi
  if echo "$cmd" | grep -qE '(aws |gcloud |az |kubectl |docker push|helm |terraform )' 2>/dev/null; then
    return 1
  fi
  # Only include npm/npx/node commands
  if echo "$cmd" | grep -qE '^(npm |npx |node )' 2>/dev/null; then
    return 0
  fi
  return 1
}

# Check if an npm script exists
npm_script_exists() {
  local script_name="$1"
  if [[ -f "${REPO_ROOT}/package.json" ]]; then
    grep -q "\"${script_name}\"" "${REPO_ROOT}/package.json" 2>/dev/null
    return $?
  fi
  return 1
}

# Extract errors/warnings from command output
extract_issues() {
  local output="$1"
  local errors=()
  local warnings=()

  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    if echo "$line" | grep -qiE '(^error|Error:|ERR!|FAIL|FAILED|✖|fatal:)'; then
      errors+=("$line")
    elif echo "$line" | grep -qiE '(^warning|Warning:|WARN|⚠)'; then
      warnings+=("$line")
    fi
  done <<< "$output"

  local err_json="["
  local first=true
  for e in "${errors[@]+"${errors[@]}"}"; do
    [[ "$first" == "true" ]] && first=false || err_json+=","
    err_json+=" \"$(json_escape "$e")\""
  done
  err_json+=" ]"

  local warn_json="["
  first=true
  for w in "${warnings[@]+"${warnings[@]}"}"; do
    [[ "$first" == "true" ]] && first=false || warn_json+=","
    warn_json+=" \"$(json_escape "$w")\""
  done
  warn_json+=" ]"

  echo "${err_json}|||${warn_json}"
}

# Process workflows
overall_status="passed"
declare -a action_required=()
results_json=""
results_first=true
workflows_found=${#WORKFLOW_FILES[@]}

for ymlfile in "${WORKFLOW_FILES[@]}"; do
  workflow_name="$(basename "$ymlfile" .yml)"
  workflow_name="$(basename "$workflow_name" .yaml)"

  # Apply workflow filter if specified
  if [[ -n "$WORKFLOW_FILTER" && "$workflow_name" != "$WORKFLOW_FILTER" ]]; then
    continue
  fi

  # Extract commands from this workflow
  declare -a commands=()
  while IFS= read -r cmd; do
    [[ -z "$cmd" ]] && continue
    commands+=("$cmd")
  done < <(extract_run_commands "$ymlfile")

  # Filter to locally runnable
  declare -a runnable_commands=()
  for cmd in "${commands[@]+"${commands[@]}"}"; do
    if is_local_runnable "$cmd"; then
      runnable_commands+=("$cmd")
    fi
  done

  if [[ ${#runnable_commands[@]} -eq 0 ]]; then
    if [[ "$results_first" == "true" ]]; then
      results_first=false
    else
      results_json+=","
    fi
    results_json+="
    {
      \"name\": \"$(json_escape "$workflow_name")\",
      \"status\": \"skipped\",
      \"reason\": \"No locally-runnable commands found\",
      \"commands\": [],
      \"errors\": [],
      \"warnings\": []
    }"
    unset commands runnable_commands
    continue
  fi

  # Run each command
  workflow_status="passed"
  declare -a cmd_results=()
  all_errors="[]"
  all_warnings="[]"

  for cmd in "${runnable_commands[@]}"; do
    # Check if npm script exists
    if [[ "$cmd" =~ ^npm\ run\ (.+)$ ]]; then
      script_name="${BASH_REMATCH[1]}"
      # Strip any flags after the script name
      script_name="${script_name%% *}"
      if ! npm_script_exists "$script_name"; then
        cmd_results+=("{\"command\": \"$(json_escape "$cmd")\", \"status\": \"skipped\", \"reason\": \"script not found\"}")
        continue
      fi
    fi

    # Run the command
    tmpout="$(mktemp)"
    tmperr="$(mktemp)"
    cmd_exit=0

    # Run from repo root
    (cd "$REPO_ROOT" && eval "$cmd" >"$tmpout" 2>"$tmperr") || cmd_exit=$?

    combined_output="$(cat "$tmpout" "$tmperr" 2>/dev/null)"
    issues="$(extract_issues "$combined_output")"
    cmd_errors="$(echo "$issues" | cut -d'|' -f1-3 | sed 's/|||$//')"
    cmd_warnings="$(echo "$issues" | sed 's/.*|||//')"

    if [[ $cmd_exit -ne 0 ]]; then
      cmd_status="failed"
      workflow_status="failed"
      overall_status="failed"
      action_required+=("${workflow_name}: '${cmd}' failed with exit code ${cmd_exit}")
    else
      cmd_status="passed"
    fi

    cmd_results+=("{\"command\": \"$(json_escape "$cmd")\", \"status\": \"${cmd_status}\", \"exitCode\": ${cmd_exit}, \"errors\": ${cmd_errors}, \"warnings\": ${cmd_warnings}}")

    rm -f "$tmpout" "$tmperr"
  done

  # Build commands array
  cmds_json="["
  cmds_first=true
  for cr in "${cmd_results[@]+"${cmd_results[@]}"}"; do
    [[ "$cmds_first" == "true" ]] && cmds_first=false || cmds_json+=","
    cmds_json+=" ${cr}"
  done
  cmds_json+=" ]"

  if [[ "$results_first" == "true" ]]; then
    results_first=false
  else
    results_json+=","
  fi
  results_json+="
    {
      \"name\": \"$(json_escape "$workflow_name")\",
      \"status\": \"${workflow_status}\",
      \"commands\": ${cmds_json}
    }"

  unset commands runnable_commands cmd_results
done

# Build actionRequired JSON array
action_json="["
action_first=true
for a in "${action_required[@]+"${action_required[@]}"}"; do
  [[ "$action_first" == "true" ]] && action_first=false || action_json+=","
  action_json+=" \"$(json_escape "$a")\""
done
action_json+=" ]"

cat <<EOF
{
  "workflowsFound": ${workflows_found},
  "results": [${results_json}
  ],
  "overallStatus": "${overall_status}",
  "actionRequired": ${action_json}
}
EOF

exit 0
