#!/bin/bash
# Vue SFC template<->script cross-reference map -> JSON to stdout
# Usage: bash scripts/variable-crossref.sh <file1.vue> [file2.vue ...]
# Or:    echo "path.vue" | bash scripts/variable-crossref.sh --stdin
# Or:    bash scripts/variable-crossref.sh --sfc-json sections.json
#
# Builds a cross-reference map between script definitions and template usage
# to prevent false-positive "unused variable" findings in code review workers.
#
# Requires: python3
# Output: JSON array with per-file crossref data

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
Usage: bash scripts/variable-crossref.sh [options] <file1.vue> [file2.vue ...]

Options:
  --stdin             Read file paths from stdin (one per line)
  --sfc-json <path>   Use pre-split sections from sfc-split.sh (avoids re-reading files)
  --summary-only      Output only crossrefSummary, not full lists
  --help              Show this help

Output: JSON array with per-file { scriptDefines[], templateReads[], crossrefSummary }.
USAGE
  exit 1
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

FROM_STDIN=false
SFC_JSON=""
SUMMARY_ONLY=false
FILES=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --stdin)
      FROM_STDIN=true
      shift
      ;;
    --sfc-json)
      [[ -z "${2:-}" ]] && die "--sfc-json requires a file path"
      SFC_JSON="$2"
      shift 2
      ;;
    --summary-only)
      SUMMARY_ONLY=true
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

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------

if ! command -v python3 &>/dev/null; then
  die "python3 is required but not found."
fi

# ---------------------------------------------------------------------------
# Build input
# ---------------------------------------------------------------------------

if [[ -n "$SFC_JSON" ]]; then
  [[ -f "$SFC_JSON" ]] || die "File not found: $SFC_JSON"
  INPUT_MODE="sfc-json"
  INPUT_DATA=$(cat "$SFC_JSON")
elif [[ ${#FILES[@]} -gt 0 ]]; then
  INPUT_MODE="files"
  INPUT_DATA=$(printf '%s\n' "${FILES[@]}")
else
  die "No input provided. Pass file paths, use --stdin, or use --sfc-json."
fi

# ---------------------------------------------------------------------------
# Cross-reference logic (python3 via heredoc — no escaping issues)
# ---------------------------------------------------------------------------

export CR_INPUT_MODE="$INPUT_MODE"
export CR_SUMMARY_ONLY="$SUMMARY_ONLY"

PYTHON_SCRIPT=$(cat <<'PYEOF'
import sys, json, re, os

input_mode = os.environ.get('CR_INPUT_MODE', 'files')
summary_only = os.environ.get('CR_SUMMARY_ONLY', 'false') == 'true'

def parse_sfc_from_file(filepath):
    """Read a .vue file and extract template + script content."""
    if not os.path.isfile(filepath):
        return None
    if not filepath.endswith('.vue'):
        return None

    with open(filepath, 'r', errors='replace') as f:
        lines = f.readlines()

    sections = {'template': None, 'script': None}
    current = None
    current_lines = []
    current_start = 0
    is_setup = False

    for i, line in enumerate(lines, 1):
        stripped = line.strip()

        if current is None:
            if re.match(r'^<template(\s|>)', stripped):
                current = 'template'
                current_start = i + 1
                current_lines = []
                continue
            if re.match(r'^<script(\s|>)', stripped):
                current = 'script'
                current_start = i + 1
                current_lines = []
                is_setup = 'setup' in stripped
                continue
        else:
            if current == 'template' and stripped == '</template>':
                sections['template'] = {
                    'content': ''.join(current_lines),
                    'startLine': current_start,
                    'endLine': i - 1,
                }
                current = None
                continue
            if current == 'script' and stripped == '</script>':
                sections['script'] = {
                    'content': ''.join(current_lines),
                    'startLine': current_start,
                    'endLine': i - 1,
                    'setup': is_setup,
                }
                current = None
                continue
            current_lines.append(line)

    return sections

def extract_script_defines(script_content, is_setup=False):
    """Extract variable/function definitions from script section."""
    defines = []
    lines = script_content.split('\n')

    for i, line in enumerate(lines, 1):
        stripped = line.strip()

        # Composition API: const X = ref(...), reactive(...), computed(...)
        m = re.match(r'const\s+(\w+)\s*=\s*(ref|shallowRef|reactive|shallowReactive|computed|toRef|toRefs)\b', stripped)
        if m:
            defines.append({'name': m.group(1), 'type': m.group(2), 'line': i})
            continue

        # Destructured: const { X, Y } = useStore() or storeToRefs(...)
        m = re.match(r'const\s*\{\s*([^}]+)\}\s*=\s*(\w+)', stripped)
        if m:
            names = [n.strip().split(':')[0].strip() for n in m.group(1).split(',')]
            for name in names:
                if name:
                    defines.append({'name': name, 'type': 'destructured', 'line': i})
            continue

        # Function declarations
        m = re.match(r'(?:async\s+)?function\s+(\w+)', stripped)
        if m:
            defines.append({'name': m.group(1), 'type': 'function', 'line': i})
            continue

        # Arrow function assignments
        m = re.match(r'const\s+(\w+)\s*=\s*(?:async\s*)?\(', stripped)
        if m:
            defines.append({'name': m.group(1), 'type': 'arrow', 'line': i})
            continue

        # defineProps — extract prop names from object keys
        if 'defineProps' in stripped:
            prop_matches = re.findall(r"['\"](\w+)['\"]", stripped)
            for name in prop_matches:
                defines.append({'name': name, 'type': 'prop', 'line': i})
            # Object-style defineProps: look for key names
            key_matches = re.findall(r'(\w+)\s*:', stripped)
            for name in key_matches:
                if name not in ('type', 'default', 'required', 'validator'):
                    defines.append({'name': name, 'type': 'prop', 'line': i})

        # defineEmits
        if 'defineEmits' in stripped:
            emit_matches = re.findall(r"['\"]([^'\"]+)['\"]", stripped)
            for name in emit_matches:
                defines.append({'name': name, 'type': 'emit', 'line': i})

    # Vuex helpers: ...mapState('module', ['prop1', 'prop2'])
    for m in re.finditer(r'\.\.\.map(?:State|Getters|Actions|Mutations)\([^,]+,\s*\[([^\]]+)\]', script_content):
        names = re.findall(r"['\"](\w+)['\"]", m.group(1))
        for name in names:
            defines.append({'name': name, 'type': 'vuex-helper', 'line': 0})

    # Options API: computed/methods/watch keys
    for section_name in ['computed', 'methods', 'watch']:
        pattern = rf'{section_name}\s*:\s*\{{'
        for m in re.finditer(pattern, script_content):
            start = m.end()
            depth = 1
            pos = start
            while pos < len(script_content) and depth > 0:
                if script_content[pos] == '{':
                    depth += 1
                elif script_content[pos] == '}':
                    depth -= 1
                pos += 1
            block = script_content[start:pos-1]
            for pm in re.finditer(r'^\s+(\w+)\s*[\(:{]', block, re.MULTILINE):
                defines.append({'name': pm.group(1), 'type': section_name, 'line': 0})

    # Deduplicate by name
    seen = set()
    unique = []
    for d in defines:
        if d['name'] not in seen:
            seen.add(d['name'])
            unique.append(d)
    return unique

def extract_template_reads(template_content):
    """Extract variable references from template section."""
    reads = []
    seen = set()

    RESERVED = {'true', 'false', 'null', 'undefined', 'typeof', 'instanceof',
                'new', 'delete', 'void', 'if', 'else', 'for', 'in', 'of',
                'return', 'class', 'const', 'let', 'var', 'this', 'item',
                'index', 'key', 'value', 'event'}

    lines = template_content.split('\n')
    for i, line in enumerate(lines, 1):
        # Interpolation: {{ expr }}
        for m in re.finditer(r'\{\{\s*([^}]+)\}\}', line):
            expr = m.group(1).strip()
            ids = re.findall(r'(?<![.\w])([a-zA-Z_]\w*)', expr)
            for ident in ids:
                if ident not in RESERVED:
                    k = (ident, 'interpolation')
                    if k not in seen:
                        seen.add(k)
                        reads.append({'name': ident, 'line': i, 'context': 'interpolation'})

        # v-if, v-show, v-else-if
        for m in re.finditer(r'v-(?:if|show|else-if)="([^"]+)"', line):
            ids = re.findall(r'(?<![.\w])([a-zA-Z_]\w*)', m.group(1))
            for ident in ids:
                if ident not in RESERVED:
                    k = (ident, 'directive')
                    if k not in seen:
                        seen.add(k)
                        reads.append({'name': ident, 'line': i, 'context': 'v-if/v-show'})

        # v-for source
        for m in re.finditer(r'v-for="[^"]*\bin\s+(\w+)', line):
            ident = m.group(1)
            k = (ident, 'v-for')
            if k not in seen:
                seen.add(k)
                reads.append({'name': ident, 'line': i, 'context': 'v-for'})

        # Bound attributes :prop="expr"
        for m in re.finditer(r'(?::|v-bind:)[\w-]+="([^"]+)"', line):
            ids = re.findall(r'(?<![.\w])([a-zA-Z_]\w*)', m.group(1))
            for ident in ids:
                if ident not in RESERVED and ident not in ('String', 'Number', 'Boolean', 'Object', 'Array'):
                    k = (ident, 'binding')
                    if k not in seen:
                        seen.add(k)
                        reads.append({'name': ident, 'line': i, 'context': 'binding'})

        # Event handlers @event="handler"
        for m in re.finditer(r'@[\w.-]+="([^"]+)"', line):
            expr = m.group(1).strip()
            ids = re.findall(r'(?<![.\w])([a-zA-Z_]\w*)', expr)
            for ident in ids:
                if ident not in RESERVED:
                    k = (ident, 'event')
                    if k not in seen:
                        seen.add(k)
                        reads.append({'name': ident, 'line': i, 'context': 'event-handler'})

    return reads

def compute_crossref(script_defines, template_reads):
    script_names = {d['name'] for d in script_defines}
    template_names = {r['name'] for r in template_reads}

    KNOWN_GLOBALS = {'store', 'router', 'route', 'emit', 'props', 'slots',
                     'attrs', 'nextTick', 'console', 'window', 'document',
                     'JSON', 'Math', 'Date', 'parseInt', 'parseFloat',
                     'setTimeout', 'clearTimeout', 'setInterval', 'clearInterval',
                     'Array', 'Object', 'String', 'Number', 'Boolean',
                     'Promise', 'Error', 'Map', 'Set'}

    used_in_both = sorted(script_names & template_names)
    used_in_script_only = sorted(script_names - template_names)
    template_refs_without_script = sorted(
        n for n in (template_names - script_names) if n not in KNOWN_GLOBALS
    )

    return {
        'usedInBoth': used_in_both,
        'usedInScriptOnly': used_in_script_only,
        'templateRefsWithoutScript': template_refs_without_script,
    }

# ── Main ────────────────────────────────────────────────────────────────────

results = []

if input_mode == 'sfc-json':
    sfc_data = json.loads(sys.stdin.read())
    for entry in sfc_data:
        filepath = entry.get('file', '')
        sections = entry.get('sections')
        if not sections:
            results.append({'file': filepath, 'crossrefSummary': None, 'reason': 'no-sections'})
            continue

        template = sections.get('template')
        script = sections.get('script')
        styles = sections.get('styles', [])

        template_content = template.get('content', '') if template else ''
        script_content = script.get('content', '') if script else ''
        is_setup = script.get('setup', False) if script else False

        script_defines = extract_script_defines(script_content, is_setup)
        template_reads = extract_template_reads(template_content)
        crossref = compute_crossref(script_defines, template_reads)

        entry_out = {'file': filepath}
        if not summary_only:
            entry_out['scriptDefines'] = script_defines
            entry_out['templateReads'] = template_reads
        entry_out['crossrefSummary'] = crossref
        results.append(entry_out)
else:
    file_list = sys.stdin.read().strip().split('\n')
    for filepath in file_list:
        filepath = filepath.strip()
        if not filepath:
            continue
        sections = parse_sfc_from_file(filepath)
        if not sections:
            results.append({'file': filepath, 'crossrefSummary': None, 'reason': 'not-vue-or-missing'})
            continue

        template = sections.get('template')
        script = sections.get('script')

        template_content = template.get('content', '') if template else ''
        script_content = script.get('content', '') if script else ''
        is_setup = script.get('setup', False) if script else False

        script_defines = extract_script_defines(script_content, is_setup)
        template_reads = extract_template_reads(template_content)
        crossref = compute_crossref(script_defines, template_reads)

        entry_out = {'file': filepath}
        if not summary_only:
            entry_out['scriptDefines'] = script_defines
            entry_out['templateReads'] = template_reads
        entry_out['crossrefSummary'] = crossref
        results.append(entry_out)

print(json.dumps(results, indent=2))
PYEOF
)

echo "$INPUT_DATA" | python3 -c "$PYTHON_SCRIPT"
