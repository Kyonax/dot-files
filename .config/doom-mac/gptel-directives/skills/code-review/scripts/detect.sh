#!/bin/bash
# Three-tier detection: brand + project + tech-stack -> JSON to stdout
# Usage: bash scripts/detect.sh [--repo-root /path]
#
# Reads the current (or specified) git repository and produces a JSON object:
#   { brand, project, techStack, signals }
# All detection is heuristic and graceful — missing git, missing package.json,
# or unrecognised patterns all fall back to "generic".

set -euo pipefail

# ── args ─────────────────────────────────────────────────────────────────────
REPO_ROOT=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo-root) REPO_ROOT="$2"; shift 2 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

if [[ -z "$REPO_ROOT" ]]; then
  REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
fi

# ── helpers ──────────────────────────────────────────────────────────────────
json_array() {
  # Turns newline-separated input into a JSON array of strings
  local items=()
  while IFS= read -r line; do
    [[ -n "$line" ]] && items+=("$line")
  done
  if [[ ${#items[@]} -eq 0 ]]; then
    echo "[]"
    return
  fi
  local out="["
  for i in "${!items[@]}"; do
    [[ $i -gt 0 ]] && out+=","
    out+="\"${items[$i]}\""
  done
  out+="]"
  echo "$out"
}

json_string_or_array() {
  # If one element -> string, else -> array
  local items=()
  while IFS= read -r line; do
    [[ -n "$line" ]] && items+=("$line")
  done
  if [[ ${#items[@]} -eq 0 ]]; then
    echo "\"generic\""
  elif [[ ${#items[@]} -eq 1 ]]; then
    echo "\"${items[0]}\""
  else
    local out="["
    for i in "${!items[@]}"; do
      [[ $i -gt 0 ]] && out+=","
      out+="\"${items[$i]}\""
    done
    out+="]"
    echo "$out"
  fi
}

# ── 1. Brand detection (from git remote) ────────────────────────────────────
BRAND="generic"
BRAND_SIGNAL=""

if REMOTE_URL="$(cd "$REPO_ROOT" && git remote get-url origin 2>/dev/null)"; then
  BRAND_SIGNAL="$REMOTE_URL"
  # Normalise: strip .git suffix, grab org/user segment
  # Works for both SSH (git@github.com:Org/repo) and HTTPS (https://github.com/Org/repo)
  ORG=""
  if [[ "$REMOTE_URL" =~ github\.com[:/]([^/]+)/ ]]; then
    ORG="${BASH_REMATCH[1]}"
  elif [[ "$REMOTE_URL" =~ gitlab\.com[:/]([^/]+)/ ]]; then
    ORG="${BASH_REMATCH[1]}"
  elif [[ "$REMOTE_URL" =~ bitbucket\.org[:/]([^/]+)/ ]]; then
    ORG="${BASH_REMATCH[1]}"
  fi

  ORG_LOWER="$(echo "$ORG" | tr '[:upper:]' '[:lower:]')"

  case "$ORG_LOWER" in
    kyonax|github-kyonax)       BRAND="kyonax" ;;
    madisonreed|madison-reed)   BRAND="madison-reed" ;;
    *)                          BRAND="generic" ;;
  esac
fi

# ── 2. Project detection (from changed file paths) ──────────────────────────
PROJECTS=()
PROJECT_SIGNALS=()

# Gather changed/untracked paths
CHANGED_FILES=""
if cd "$REPO_ROOT" 2>/dev/null; then
  STAGED="$(git diff --cached --name-only 2>/dev/null || true)"
  UNSTAGED="$(git diff --name-only 2>/dev/null || true)"
  UNTRACKED="$(git ls-files --others --exclude-standard 2>/dev/null || true)"
  CHANGED_FILES="$(printf '%s\n%s\n%s' "$STAGED" "$UNSTAGED" "$UNTRACKED" | sort -u | grep -v '^$' || true)"
fi

detect_project_from_path() {
  local path="$1"
  case "$path" in
    website/src/vuescripts/*|website/src/views/*|website/src/routing/*)
      echo "mr-dotcom" ;;
    mr_modules/*|apiserver/*|tophat/*)
      echo "mr-backend" ;;
    *sources/hud/*|*@*/sources/hud/*)
      echo "kyonax-obs-hud" ;;
    *)
      echo "" ;;
  esac
}

if [[ -n "$CHANGED_FILES" ]]; then
  while IFS= read -r filepath; do
    proj="$(detect_project_from_path "$filepath")"
    if [[ -n "$proj" ]]; then
      # Deduplicate
      already=false
      for existing in "${PROJECTS[@]+"${PROJECTS[@]}"}"; do
        [[ "$existing" == "$proj" ]] && already=true && break
      done
      if [[ "$already" == false ]]; then
        PROJECTS+=("$proj")
        PROJECT_SIGNALS+=("$filepath")
      fi
    fi
  done <<< "$CHANGED_FILES"
fi

# Fallback
if [[ ${#PROJECTS[@]} -eq 0 ]]; then
  PROJECTS=("generic")
fi

# ── 3. Tech-stack detection ─────────────────────────────────────────────────
TECH_STACK=()
TECH_SIGNALS=()

# Search for package.json files (root + one level deep)
PKG_FILES=()
if [[ -f "$REPO_ROOT/package.json" ]]; then
  PKG_FILES+=("$REPO_ROOT/package.json")
fi
for subpkg in "$REPO_ROOT"/*/package.json; do
  [[ -f "$subpkg" ]] && PKG_FILES+=("$subpkg")
done

add_tech() {
  local tech="$1" signal="$2"
  for existing in "${TECH_STACK[@]+"${TECH_STACK[@]}"}"; do
    [[ "$existing" == "$tech" ]] && return
  done
  TECH_STACK+=("$tech")
  TECH_SIGNALS+=("$signal")
}

for pkg in "${PKG_FILES[@]+"${PKG_FILES[@]}"}"; do
  if grep -q '"vue"' "$pkg" 2>/dev/null; then
    add_tech "vue3" "$pkg"
  fi
  if grep -q '"express"' "$pkg" 2>/dev/null; then
    add_tech "express" "$pkg"
  fi
  if grep -q '"react"' "$pkg" 2>/dev/null; then
    add_tech "react" "$pkg"
  fi
  if grep -q '"next"' "$pkg" 2>/dev/null; then
    add_tech "next" "$pkg"
  fi
done

# Composition API detection from changed files (additive to vue3)
if [[ -n "$CHANGED_FILES" ]]; then
  COMPOSITION_DETECTED=false
  while IFS= read -r filepath; do
    fullpath="$REPO_ROOT/$filepath"
    if [[ -f "$fullpath" ]]; then
      if grep -q '<script setup>' "$fullpath" 2>/dev/null || \
         grep -q '@vue/composition-api' "$fullpath" 2>/dev/null || \
         grep -q 'defineComponent' "$fullpath" 2>/dev/null; then
        COMPOSITION_DETECTED=true
        break
      fi
    fi
  done <<< "$CHANGED_FILES"

  if [[ "$COMPOSITION_DETECTED" == true ]]; then
    add_tech "vue3-composition" "changed-files"
  fi
fi

# Fallback
if [[ ${#TECH_STACK[@]} -eq 0 ]]; then
  TECH_STACK=("generic")
fi

# ── 4. Build signals object ─────────────────────────────────────────────────
# Collect meaningful signals for debugging/tracing
SIGNAL_REMOTE="${BRAND_SIGNAL:-none}"
SIGNAL_PROJECTS="$(printf '%s\n' "${PROJECT_SIGNALS[@]+"${PROJECT_SIGNALS[@]}"}" | head -5 | tr '\n' ',' | sed 's/,$//')"
SIGNAL_TECH="$(printf '%s\n' "${TECH_SIGNALS[@]+"${TECH_SIGNALS[@]}"}" | head -5 | tr '\n' ',' | sed 's/,$//')"
CHANGED_COUNT="$(echo "$CHANGED_FILES" | grep -c '[^[:space:]]' || echo 0)"

# ── 5. Emit JSON ────────────────────────────────────────────────────────────
PROJECT_JSON="$(printf '%s\n' "${PROJECTS[@]}" | json_string_or_array)"
TECH_JSON="$(printf '%s\n' "${TECH_STACK[@]}" | json_array)"

cat <<ENDJSON
{
  "brand": "$BRAND",
  "project": $PROJECT_JSON,
  "techStack": $TECH_JSON,
  "signals": {
    "remote": "$SIGNAL_REMOTE",
    "projectPaths": "$SIGNAL_PROJECTS",
    "techSources": "$SIGNAL_TECH",
    "changedFileCount": $CHANGED_COUNT,
    "repoRoot": "$REPO_ROOT"
  }
}
ENDJSON
