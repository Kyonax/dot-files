#!/bin/bash
# Split Vue SFC files into template/script/style sections -> JSON to stdout
# Usage: bash scripts/sfc-split.sh <file1.vue> [file2.vue ...]
# Or:    echo "path/to/file.vue" | bash scripts/sfc-split.sh --stdin
#
# Output: JSON array with per-file sections (template, script, styles).
# Each section includes lang, content, startLine, endLine, and byteSize.
# Non-Vue files produce { sections: null, reason: "not-vue" }.

set -euo pipefail

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

die() {
  echo "ERROR: $*" >&2
  exit 1
}

json_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="${s//$'\n'/\\n}"
  s="${s//$'\t'/\\t}"
  s="${s//$'\r'/\\r}"
  printf '%s' "$s"
}

usage() {
  cat <<'USAGE'
Usage: bash scripts/sfc-split.sh [options] <file1.vue> [file2.vue ...]

Options:
  --stdin           Read file paths from stdin (one per line)
  --help            Show this help

Output: JSON array to stdout with per-file section data.
USAGE
  exit 1
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

FROM_STDIN=false
FILES=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --stdin)
      FROM_STDIN=true
      shift
      ;;
    --help|-h)
      usage
      ;;
    *)
      FILES+=("$1")
      shift
      ;;
  esac
done

if $FROM_STDIN; then
  while IFS= read -r line; do
    [[ -n "$line" ]] && FILES+=("$line")
  done
fi

if [[ ${#FILES[@]} -eq 0 ]]; then
  die "No files provided. Pass file paths as arguments or use --stdin."
fi

# ---------------------------------------------------------------------------
# SFC parsing function
# ---------------------------------------------------------------------------

parse_sfc() {
  local filepath="$1"

  # Check file exists and is .vue
  if [[ ! -f "$filepath" ]]; then
    echo "{\"file\":\"$(json_escape "$filepath")\",\"sections\":null,\"reason\":\"file-not-found\"}"
    return
  fi

  local ext="${filepath##*.}"
  if [[ "$ext" != "vue" ]]; then
    echo "{\"file\":\"$(json_escape "$filepath")\",\"sections\":null,\"reason\":\"not-vue\"}"
    return
  fi

  # State machine: track which section we're in
  local current_section=""    # template | script | scriptSetup | style
  local current_lang=""
  local current_scoped=""
  local current_setup=""
  local section_start=0
  local line_num=0

  # Accumulators
  local template_lang=""
  local template_start=0
  local template_end=0
  local template_content=""

  local script_lang=""
  local script_setup="false"
  local script_start=0
  local script_end=0
  local script_content=""

  # Styles can be multiple
  local -a style_entries=()
  local style_lang=""
  local style_scoped="false"
  local style_start=0
  local style_content=""

  while IFS= read -r line || [[ -n "$line" ]]; do
    line_num=$((line_num + 1))

    # Detect section opening tags
    if [[ -z "$current_section" ]]; then
      # Template opening
      if [[ "$line" =~ ^\<template([[:space:]]|>) ]]; then
        current_section="template"
        template_start=$((line_num + 1))
        # Extract lang attribute
        if [[ "$line" =~ lang=\"([^\"]+)\" ]]; then
          template_lang="${BASH_REMATCH[1]}"
        else
          template_lang="html"
        fi
        continue
      fi

      # Script opening
      if [[ "$line" =~ ^\<script([[:space:]]|>) ]]; then
        current_section="script"
        script_start=$((line_num + 1))
        if [[ "$line" =~ lang=\"([^\"]+)\" ]]; then
          script_lang="${BASH_REMATCH[1]}"
        else
          script_lang="js"
        fi
        if [[ "$line" =~ setup ]]; then
          script_setup="true"
        else
          script_setup="false"
        fi
        continue
      fi

      # Style opening
      if [[ "$line" =~ ^\<style([[:space:]]|>) ]]; then
        current_section="style"
        style_start=$((line_num + 1))
        if [[ "$line" =~ lang=\"([^\"]+)\" ]]; then
          style_lang="${BASH_REMATCH[1]}"
        else
          style_lang="css"
        fi
        if [[ "$line" =~ scoped ]]; then
          style_scoped="true"
        else
          style_scoped="false"
        fi
        style_content=""
        continue
      fi
    fi

    # Detect section closing tags
    if [[ "$current_section" == "template" ]] && [[ "$line" =~ ^\</template\> ]]; then
      template_end=$((line_num - 1))
      current_section=""
      continue
    fi

    if [[ "$current_section" == "script" ]] && [[ "$line" =~ ^\</script\> ]]; then
      script_end=$((line_num - 1))
      current_section=""
      continue
    fi

    if [[ "$current_section" == "style" ]] && [[ "$line" =~ ^\</style\> ]]; then
      local style_end_line=$((line_num - 1))
      local escaped_content
      escaped_content=$(json_escape "$style_content")
      local byte_size=${#style_content}
      style_entries+=("{\"lang\":\"$style_lang\",\"scoped\":$style_scoped,\"startLine\":$style_start,\"endLine\":$style_end_line,\"content\":\"$escaped_content\",\"byteSize\":$byte_size}")
      current_section=""
      style_content=""
      continue
    fi

    # Accumulate content
    case "$current_section" in
      template)
        if [[ -n "$template_content" ]]; then
          template_content+=$'\n'"$line"
        else
          template_content="$line"
        fi
        ;;
      script)
        if [[ -n "$script_content" ]]; then
          script_content+=$'\n'"$line"
        else
          script_content="$line"
        fi
        ;;
      style)
        if [[ -n "$style_content" ]]; then
          style_content+=$'\n'"$line"
        else
          style_content="$line"
        fi
        ;;
    esac
  done < "$filepath"

  # Build JSON output
  local template_json="null"
  if [[ $template_start -gt 0 ]] && [[ $template_end -gt 0 ]]; then
    local t_escaped
    t_escaped=$(json_escape "$template_content")
    local t_bytes=${#template_content}
    template_json="{\"lang\":\"$template_lang\",\"startLine\":$template_start,\"endLine\":$template_end,\"content\":\"$t_escaped\",\"byteSize\":$t_bytes}"
  fi

  local script_json="null"
  if [[ $script_start -gt 0 ]] && [[ $script_end -gt 0 ]]; then
    local s_escaped
    s_escaped=$(json_escape "$script_content")
    local s_bytes=${#script_content}
    script_json="{\"lang\":\"$script_lang\",\"setup\":$script_setup,\"startLine\":$script_start,\"endLine\":$script_end,\"content\":\"$s_escaped\",\"byteSize\":$s_bytes}"
  fi

  local styles_json="[]"
  if [[ ${#style_entries[@]} -gt 0 ]]; then
    local joined
    joined=$(IFS=,; echo "${style_entries[*]}")
    styles_json="[$joined]"
  fi

  echo "{\"file\":\"$(json_escape "$filepath")\",\"totalLines\":$line_num,\"sections\":{\"template\":$template_json,\"script\":$script_json,\"styles\":$styles_json}}"
}

# ---------------------------------------------------------------------------
# Main: process all files, output JSON array
# ---------------------------------------------------------------------------

echo "["

first=true
for f in "${FILES[@]}"; do
  if $first; then
    first=false
  else
    echo ","
  fi
  parse_sfc "$f"
done

echo "]"
