#!/bin/bash
# Structured diff + context per changed file -> JSON to stdout
# Usage: bash scripts/diff-context.sh [--base master] [--pr-diff /path/to/pr-diff/]

set -euo pipefail

BASE="master"
PR_DIFF=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --base) BASE="$2"; shift 2 ;;
    --pr-diff) PR_DIFF="$2"; shift 2 ;;
    *) echo "Unknown arg: $1" >&2; exit 1 ;;
  esac
done

# Map file extension to language name
ext_to_lang() {
  local ext="${1##*.}"
  case "$ext" in
    js) echo "javascript" ;;
    ts) echo "typescript" ;;
    tsx) echo "tsx" ;;
    jsx) echo "jsx" ;;
    vue) echo "vue" ;;
    css) echo "css" ;;
    styl) echo "stylus" ;;
    scss) echo "scss" ;;
    less) echo "less" ;;
    html) echo "html" ;;
    pug) echo "pug" ;;
    json) echo "json" ;;
    yml|yaml) echo "yaml" ;;
    md) echo "markdown" ;;
    sh|bash) echo "bash" ;;
    py) echo "python" ;;
    rb) echo "ruby" ;;
    go) echo "go" ;;
    rs) echo "rust" ;;
    java) echo "java" ;;
    *) echo "$ext" ;;
  esac
}

# Escape a string for safe JSON embedding
json_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="$(printf '%s' "$s" | sed ':a;N;$!ba;s/\n/\\n/g')"
  s="$(printf '%s' "$s" | sed 's/\t/\\t/g')"
  printf '%s' "$s"
}

# Extract changed line numbers from unified diff @@ hunks
# Parses the new-file side: @@ -old,count +new,count @@
extract_changed_lines() {
  local diff_text="$1"
  local lines=()
  while IFS= read -r hunk; do
    local start count
    start="$(echo "$hunk" | sed -n 's/^@@ -[0-9,]* +\([0-9]*\).*/\1/p')"
    count="$(echo "$hunk" | sed -n 's/^@@ -[0-9,]* +[0-9]*,\([0-9]*\).*/\1/p')"
    if [[ -z "$count" ]]; then
      count=1
    fi
    if [[ -n "$start" ]]; then
      local end=$((start + count - 1))
      lines+=("${start}-${end}")
    fi
  done < <(echo "$diff_text" | grep '^@@')
  local IFS=','
  echo "${lines[*]}"
}

# Collect file list and diff text
declare -a FILES=()
declare -A DIFFS=()

if [[ -n "$PR_DIFF" ]]; then
  # Read diffs from directory of patch files
  if [[ ! -d "$PR_DIFF" ]]; then
    echo '{"error": "pr-diff directory not found: '"$(json_escape "$PR_DIFF")"'"}'
    exit 1
  fi
  for patchfile in "$PR_DIFF"/*; do
    [[ -f "$patchfile" ]] || continue
    # Extract the file path from the diff header (--- a/path or +++ b/path)
    filepath="$(grep -m1 '^+++ b/' "$patchfile" 2>/dev/null | sed 's|^+++ b/||')"
    if [[ -z "$filepath" ]]; then
      # Fallback: use the patch filename itself
      filepath="$(basename "$patchfile")"
    fi
    FILES+=("$filepath")
    DIFFS["$filepath"]="$(cat "$patchfile")"
  done
else
  # Use git diff against base branch
  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    FILES+=("$f")
  done < <(git diff --name-only "${BASE}...HEAD" 2>/dev/null || git diff --name-only "${BASE}" HEAD 2>/dev/null || git diff --name-only HEAD 2>/dev/null)

  for f in "${FILES[@]}"; do
    DIFFS["$f"]="$(git diff --unified=5 HEAD -- "$f" 2>/dev/null || true)"
  done
fi

# Build JSON output
echo "["

first=true
for filepath in "${FILES[@]}"; do
  diff_text="${DIFFS[$filepath]:-}"
  lang="$(ext_to_lang "$filepath")"

  # Extract imports
  imports=""
  if [[ -f "$filepath" ]]; then
    imports="$(grep -nE '^\s*(import\s|const\s.*=\s*require\(|let\s.*=\s*require\(|var\s.*=\s*require\()' "$filepath" 2>/dev/null | head -50 || true)"
  fi

  # Extract exports
  exports=""
  if [[ -f "$filepath" ]]; then
    exports="$(grep -nE '^\s*(export\s|module\.exports)' "$filepath" 2>/dev/null | head -50 || true)"
  fi

  # Changed line ranges
  changed_lines="$(extract_changed_lines "$diff_text")"

  # Vue-specific: component name and store modules
  component_name=""
  store_modules=""
  if [[ "$lang" == "vue" && -f "$filepath" ]]; then
    # Component name from `name:` property or filename
    component_name="$(grep -m1 "name:" "$filepath" 2>/dev/null | sed "s/.*name:[[:space:]]*['\"]\\([^'\"]*\\)['\"].*/\\1/" || true)"
    if [[ -z "$component_name" || "$component_name" == *"name:"* ]]; then
      component_name="$(basename "$filepath" .vue)"
    fi
    # Store usage
    store_modules="$(grep -nE '(mapState|mapGetters|mapActions|mapMutations|useStore|store\.state|store\.dispatch|store\.commit|store\.getters|storeToRefs)' "$filepath" 2>/dev/null | head -30 || true)"
  fi

  # Emit JSON object
  if [[ "$first" == "true" ]]; then
    first=false
  else
    echo ","
  fi

  cat <<JSONOBJ
  {
    "path": "$(json_escape "$filepath")",
    "language": "$(json_escape "$lang")",
    "diff": "$(json_escape "$diff_text")",
    "imports": "$(json_escape "$imports")",
    "exports": "$(json_escape "$exports")",
    "changedLineRanges": "$(json_escape "$changed_lines")",
    "componentName": "$(json_escape "$component_name")",
    "storeModules": "$(json_escape "$store_modules")"
  }
JSONOBJ

done

echo ""
echo "]"
