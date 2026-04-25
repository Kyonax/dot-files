#!/bin/bash
# Vue component hierarchy -> JSON to stdout
# Usage: bash scripts/component-tree.sh <file.vue>

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo '{"error": "Usage: component-tree.sh <file.vue>"}' >&2
  exit 1
fi

VUE_FILE="$1"

if [[ ! -f "$VUE_FILE" ]]; then
  echo '{"error": "File not found: '"$VUE_FILE"'"}'
  exit 1
fi

# Escape a string for safe JSON embedding
json_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="$(printf '%s' "$s" | sed ':a;N;$!ba;s/\n/\\n/g')"
  s="$(printf '%s' "$s" | sed 's/\t/\\t/g')"
  printf '%s' "$s"
}

# Derive root component name from filename or name: property
root_name="$(grep -m1 "name:" "$VUE_FILE" 2>/dev/null | sed "s/.*name:[[:space:]]*['\"]\\([^'\"]*\\)['\"].*/\\1/" || true)"
if [[ -z "$root_name" || "$root_name" == *"name:"* ]]; then
  root_name="$(basename "$VUE_FILE" .vue)"
fi

# Extract component imports
# Patterns: import FooBar from '...', import FooBar from "..."
# Also: components ending with .vue in the path
declare -a children=()
while IFS= read -r line; do
  [[ -z "$line" ]] && continue
  # Extract the imported identifier
  comp="$(echo "$line" | sed -n "s/.*import[[:space:]]\+\([A-Z][A-Za-z0-9]*\)[[:space:]]\+from.*/\1/p")"
  if [[ -n "$comp" ]]; then
    children+=("$comp")
    continue
  fi
  # Catch: import { X } from or named imports of PascalCase identifiers
  named="$(echo "$line" | sed -n 's/.*import[[:space:]]*{[[:space:]]*\(.*\)[[:space:]]*}[[:space:]]*from.*/\1/p')"
  if [[ -n "$named" ]]; then
    IFS=',' read -ra parts <<< "$named"
    for part in "${parts[@]}"; do
      part="$(echo "$part" | xargs)"  # trim whitespace
      # Only include PascalCase names (likely components)
      if [[ "$part" =~ ^[A-Z][a-zA-Z0-9]+$ ]]; then
        children+=("$part")
      fi
    done
  fi
done < <(grep -E "import\s.*from\s" "$VUE_FILE" 2>/dev/null | grep -iE '([A-Z][a-zA-Z0-9]+|\.vue)' || true)

# Also check require() style component imports
while IFS= read -r line; do
  comp="$(echo "$line" | sed -n "s/.*\(const\|let\|var\)[[:space:]]\+\([A-Z][A-Za-z0-9]*\)[[:space:]]*=.*/\2/p")"
  if [[ -n "$comp" ]]; then
    children+=("$comp")
  fi
done < <(grep -E '(const|let|var)\s+[A-Z][a-zA-Z0-9]+\s*=\s*require\(' "$VUE_FILE" 2>/dev/null || true)

# Extract composable usage (functions starting with `use`)
declare -a composables=()
while IFS= read -r line; do
  # Pattern: useXxx( or const x = useXxx(
  while [[ "$line" =~ (use[A-Z][a-zA-Z0-9]*) ]]; do
    comp="${BASH_REMATCH[1]}"
    # Avoid duplicates
    dupe=false
    for existing in "${composables[@]+"${composables[@]}"}"; do
      if [[ "$existing" == "$comp" ]]; then
        dupe=true
        break
      fi
    done
    if [[ "$dupe" == "false" ]]; then
      composables+=("$comp")
    fi
    # Remove matched part to find more in same line
    line="${line#*"$comp"}"
  done
done < <(grep -E 'use[A-Z][a-zA-Z0-9]*' "$VUE_FILE" 2>/dev/null || true)

# Extract store reads and dispatches
declare -a store_reads=()
declare -a store_dispatches=()

# mapState / mapGetters / store.state / store.getters / storeToRefs -> reads
while IFS= read -r line; do
  if echo "$line" | grep -qE '(mapState|mapGetters|store\.state|store\.getters|storeToRefs)'; then
    # Extract the module/store name if present
    module="$(echo "$line" | sed -n "s/.*\(mapState\|mapGetters\|storeToRefs\)[[:space:]]*([[:space:]]*['\"]\\([^'\"]*\\)['\"].*/\2/p")"
    if [[ -n "$module" ]]; then
      dupe=false
      for existing in "${store_reads[@]+"${store_reads[@]}"}"; do
        [[ "$existing" == "$module" ]] && dupe=true && break
      done
      [[ "$dupe" == "false" ]] && store_reads+=("$module")
    fi
    # Also capture store.state.xxx
    state_mod="$(echo "$line" | sed -n 's/.*store\.state\.\([a-zA-Z0-9_]*\).*/\1/p')"
    if [[ -n "$state_mod" ]]; then
      dupe=false
      for existing in "${store_reads[@]+"${store_reads[@]}"}"; do
        [[ "$existing" == "$state_mod" ]] && dupe=true && break
      done
      [[ "$dupe" == "false" ]] && store_reads+=("$state_mod")
    fi
    # store.getters.xxx or store.getters['xxx']
    getter_mod="$(echo "$line" | sed -n "s/.*store\.getters\.\([a-zA-Z0-9_]*\).*/\1/p")"
    if [[ -z "$getter_mod" ]]; then
      getter_mod="$(echo "$line" | sed -n "s/.*store\.getters\['\([^']*\)'\].*/\1/p")"
    fi
    if [[ -n "$getter_mod" ]]; then
      dupe=false
      for existing in "${store_reads[@]+"${store_reads[@]}"}"; do
        [[ "$existing" == "$getter_mod" ]] && dupe=true && break
      done
      [[ "$dupe" == "false" ]] && store_reads+=("$getter_mod")
    fi
  fi
done < <(grep -nE '(mapState|mapGetters|store\.state|store\.getters|storeToRefs)' "$VUE_FILE" 2>/dev/null || true)

# mapActions / mapMutations / store.dispatch / store.commit -> dispatches
while IFS= read -r line; do
  if echo "$line" | grep -qE '(mapActions|mapMutations|store\.dispatch|store\.commit)'; then
    module="$(echo "$line" | sed -n "s/.*\(mapActions\|mapMutations\)[[:space:]]*([[:space:]]*['\"]\\([^'\"]*\\)['\"].*/\2/p")"
    if [[ -n "$module" ]]; then
      dupe=false
      for existing in "${store_dispatches[@]+"${store_dispatches[@]}"}"; do
        [[ "$existing" == "$module" ]] && dupe=true && break
      done
      [[ "$dupe" == "false" ]] && store_dispatches+=("$module")
    fi
    # store.dispatch('module/action') or store.dispatch('action')
    dispatch_target="$(echo "$line" | sed -n "s/.*store\.dispatch([[:space:]]*['\"]\\([^'\"]*\\)['\"].*/\1/p")"
    if [[ -n "$dispatch_target" ]]; then
      dupe=false
      for existing in "${store_dispatches[@]+"${store_dispatches[@]}"}"; do
        [[ "$existing" == "$dispatch_target" ]] && dupe=true && break
      done
      [[ "$dupe" == "false" ]] && store_dispatches+=("$dispatch_target")
    fi
    # store.commit('module/mutation') or store.commit('mutation')
    commit_target="$(echo "$line" | sed -n "s/.*store\.commit([[:space:]]*['\"]\\([^'\"]*\\)['\"].*/\1/p")"
    if [[ -n "$commit_target" ]]; then
      dupe=false
      for existing in "${store_dispatches[@]+"${store_dispatches[@]}"}"; do
        [[ "$existing" == "$commit_target" ]] && dupe=true && break
      done
      [[ "$dupe" == "false" ]] && store_dispatches+=("$commit_target")
    fi
  fi
done < <(grep -nE '(mapActions|mapMutations|store\.dispatch|store\.commit)' "$VUE_FILE" 2>/dev/null || true)

# Build JSON arrays as strings
children_json="["
first=true
for c in "${children[@]+"${children[@]}"}"; do
  [[ "$first" == "true" ]] && first=false || children_json+=","
  children_json+=" \"$(json_escape "$c")\""
done
children_json+=" ]"

composables_json="["
first=true
for c in "${composables[@]+"${composables[@]}"}"; do
  [[ "$first" == "true" ]] && first=false || composables_json+=","
  composables_json+=" \"$(json_escape "$c")\""
done
composables_json+=" ]"

reads_json="["
first=true
for r in "${store_reads[@]+"${store_reads[@]}"}"; do
  [[ "$first" == "true" ]] && first=false || reads_json+=","
  reads_json+=" \"$(json_escape "$r")\""
done
reads_json+=" ]"

dispatches_json="["
first=true
for d in "${store_dispatches[@]+"${store_dispatches[@]}"}"; do
  [[ "$first" == "true" ]] && first=false || dispatches_json+=","
  dispatches_json+=" \"$(json_escape "$d")\""
done
dispatches_json+=" ]"

cat <<EOF
{
  "root": "$(json_escape "$root_name")",
  "file": "$(json_escape "$VUE_FILE")",
  "children": ${children_json},
  "composables": ${composables_json},
  "storeReads": ${reads_json},
  "storeDispatches": ${dispatches_json}
}
EOF
