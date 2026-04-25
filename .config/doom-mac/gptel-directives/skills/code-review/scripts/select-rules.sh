#!/bin/bash
# Two-pass rule selection -> newline-separated file paths to stdout
# Usage: bash scripts/select-rules.sh [--context context.json] < detect.json
#
# Pass 1: directory-level selection from detection JSON (brand, project, techStack)
# Pass 2: tag-based relevance filtering against diff keywords (optional)
#
# Output: one rule-file path per line, relative to the skill root.

set -euo pipefail

# ── resolve skill root ──────────────────────────────────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SKILL_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# ── args ─────────────────────────────────────────────────────────────────────
CONTEXT_FILE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --context) CONTEXT_FILE="$2"; shift 2 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

# ── read detection JSON from stdin ───────────────────────────────────────────
DETECT_JSON="$(cat)"

# Minimal JSON parsing with grep/sed (no jq dependency)
extract_json_string() {
  # Extract a simple string value: "key": "value"
  echo "$DETECT_JSON" | grep -o "\"$1\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" | head -1 | sed 's/.*:[[:space:]]*"\([^"]*\)"/\1/'
}

extract_json_array_items() {
  # Extract items from a JSON array for a given key
  # Handles both "key": "single" and "key": ["a","b"]
  local key="$1"
  local raw
  raw="$(echo "$DETECT_JSON" | grep -o "\"$key\"[[:space:]]*:[[:space:]]*\[[^]]*\]" | head -1 | sed "s/\"$key\"[[:space:]]*:[[:space:]]*//")"
  if [[ -n "$raw" ]]; then
    # It's an array — extract quoted strings
    echo "$raw" | grep -o '"[^"]*"' | sed 's/"//g'
  else
    # Try as a plain string
    local val
    val="$(extract_json_string "$key")"
    [[ -n "$val" ]] && echo "$val"
  fi
}

BRAND="$(extract_json_string "brand")"
TECH_STACKS=()
while IFS= read -r item; do
  [[ -n "$item" ]] && TECH_STACKS+=("$item")
done < <(extract_json_array_items "techStack")

PROJECTS=()
while IFS= read -r item; do
  [[ -n "$item" ]] && PROJECTS+=("$item")
done < <(extract_json_array_items "project")

# ── Pass 1: directory-level collection ───────────────────────────────────────
collect_rules() {
  # Collect all .md files under a directory, excluding INDEX.md
  local dir="$1"
  if [[ -d "$dir" ]]; then
    find "$dir" -name '*.md' -not -name 'INDEX.md' -type f 2>/dev/null | sort
  fi
}

relative_to_skill() {
  # Convert absolute path to path relative to skill root
  local abs="$1"
  echo "${abs#$SKILL_DIR/}"
}

COLLECTED_FILES=()

# Always collect universal rules
while IFS= read -r f; do
  COLLECTED_FILES+=("$f")
done < <(collect_rules "$SKILL_DIR/universal")

# Framework rules based on detected tech stacks
for stack in "${TECH_STACKS[@]+"${TECH_STACKS[@]}"}"; do
  [[ "$stack" == "generic" ]] && continue
  while IFS= read -r f; do
    COLLECTED_FILES+=("$f")
  done < <(collect_rules "$SKILL_DIR/framework/$stack")
done

# Brand rules
if [[ -n "$BRAND" && "$BRAND" != "generic" ]]; then
  while IFS= read -r f; do
    COLLECTED_FILES+=("$f")
  done < <(collect_rules "$SKILL_DIR/brand/$BRAND")
fi

# Project rules
for proj in "${PROJECTS[@]+"${PROJECTS[@]}"}"; do
  [[ "$proj" == "generic" ]] && continue
  while IFS= read -r f; do
    COLLECTED_FILES+=("$f")
  done < <(collect_rules "$SKILL_DIR/project/$proj")
done

# Deduplicate
declare -A SEEN
PASS1_FILES=()
for f in "${COLLECTED_FILES[@]+"${COLLECTED_FILES[@]}"}"; do
  if [[ -z "${SEEN[$f]+_}" ]]; then
    SEEN["$f"]=1
    PASS1_FILES+=("$f")
  fi
done

# ── Pass 2: tag-based relevance filtering (optional) ────────────────────────
if [[ -n "$CONTEXT_FILE" && -f "$CONTEXT_FILE" ]]; then
  # Extract keywords from context JSON
  # Context JSON is expected to contain diff content or file paths
  # We extract anything that looks like a code keyword:
  #   aria-*, v-if, v-for, h1-h6, role-*, display, flex, padding, margin,
  #   function/class/variable names, HTML tags, CSS properties, etc.
  CONTEXT_CONTENT="$(cat "$CONTEXT_FILE")"

  # Build keyword set from context: extract words that could be code-relevant
  KEYWORDS=()
  while IFS= read -r kw; do
    [[ -n "$kw" ]] && KEYWORDS+=("$kw")
  done < <(
    echo "$CONTEXT_CONTENT" | \
    grep -oE '[a-zA-Z][a-zA-Z0-9_-]*' | \
    tr '[:upper:]' '[:lower:]' | \
    sort -u
  )

  # Also extract compound patterns like aria-label, v-if, h1-h6, role-alert
  while IFS= read -r kw; do
    [[ -n "$kw" ]] && KEYWORDS+=("$kw")
  done < <(
    echo "$CONTEXT_CONTENT" | \
    grep -oE '(aria|role|v|h)[a-zA-Z0-9_-]+' | \
    tr '[:upper:]' '[:lower:]' | \
    sort -u
  )

  # Build a lookup set for fast matching
  declare -A KEYWORD_SET
  for kw in "${KEYWORDS[@]+"${KEYWORDS[@]}"}"; do
    KEYWORD_SET["$kw"]=1
  done

  # Filter: keep rule only if at least 1 tag matches a keyword
  PASS2_FILES=()
  for f in "${PASS1_FILES[@]+"${PASS1_FILES[@]}"}"; do
    # Extract tags from frontmatter
    TAGS_LINE=""
    IN_FRONTMATTER=false
    while IFS= read -r line; do
      if [[ "$line" == "---" ]]; then
        if [[ "$IN_FRONTMATTER" == true ]]; then
          break
        else
          IN_FRONTMATTER=true
          continue
        fi
      fi
      if [[ "$IN_FRONTMATTER" == true ]] && [[ "$line" =~ ^tags: ]]; then
        TAGS_LINE="${line#tags:}"
        break
      fi
    done < "$f"

    if [[ -z "$TAGS_LINE" ]]; then
      # No tags — include by default (can't filter what has no metadata)
      PASS2_FILES+=("$f")
      continue
    fi

    # Parse comma-separated tags, trim whitespace
    MATCHED=false
    while IFS=',' read -ra TAG_ITEMS; do
      for tag in "${TAG_ITEMS[@]}"; do
        # Trim whitespace and lowercase
        tag="$(echo "$tag" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | tr '[:upper:]' '[:lower:]')"
        [[ -z "$tag" ]] && continue
        if [[ -n "${KEYWORD_SET[$tag]+_}" ]]; then
          MATCHED=true
          break
        fi
        # Also try matching individual words within multi-word tags
        for word in $tag; do
          if [[ -n "${KEYWORD_SET[$word]+_}" ]]; then
            MATCHED=true
            break 2
          fi
        done
      done
    done <<< "$TAGS_LINE"

    if [[ "$MATCHED" == true ]]; then
      PASS2_FILES+=("$f")
    fi
  done

  # Output pass 2 results
  for f in "${PASS2_FILES[@]+"${PASS2_FILES[@]}"}"; do
    relative_to_skill "$f"
  done
else
  # No context filtering — output all pass 1 results
  for f in "${PASS1_FILES[@]+"${PASS1_FILES[@]}"}"; do
    relative_to_skill "$f"
  done
fi
