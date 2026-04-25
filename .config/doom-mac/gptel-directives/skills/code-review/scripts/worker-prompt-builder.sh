#!/bin/bash
# Assemble targeted context per worker from all pre-computed JSON -> per-worker JSON files
# Usage: bash scripts/worker-prompt-builder.sh --workers w.json --sfc s.json --crossref c.json --digest d.json --context ctx.json [--output-dir /tmp/cr-worker-prompts]
#
# Reads outputs from: worker-dispatch.sh, sfc-split.sh, variable-crossref.sh,
# pr-review-digest.sh, diff-context.sh. Produces one JSON file per worker with
# only the code sections, crossref hints, digest items, and diff hunks relevant
# to that specific worker's rule category.
#
# Requires: python3
# Output: One JSON file per worker in --output-dir

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
Usage: bash scripts/worker-prompt-builder.sh [options]

Required:
  --workers <path>     workers.json from worker-dispatch.sh
  --context <path>     context.json from diff-context.sh

Optional (enhance context, reduce tokens):
  --sfc <path>         sections.json from sfc-split.sh
  --crossref <path>    crossref.json from variable-crossref.sh
  --digest <path>      digest.json from pr-review-digest.sh
  --output-dir <path>  Directory for per-worker JSON files (default: /tmp/cr-worker-prompts)
  --help               Show this help

Output: Per-worker JSON files in --output-dir, plus summary JSON to stdout.
USAGE
  exit 1
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

WORKERS_JSON=""
SFC_JSON=""
CROSSREF_JSON=""
DIGEST_JSON=""
CONTEXT_JSON=""
OUTPUT_DIR="/tmp/cr-worker-prompts"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --workers)
      [[ -z "${2:-}" ]] && die "--workers requires a file path"
      WORKERS_JSON="$2"
      shift 2
      ;;
    --sfc)
      [[ -z "${2:-}" ]] && die "--sfc requires a file path"
      SFC_JSON="$2"
      shift 2
      ;;
    --crossref)
      [[ -z "${2:-}" ]] && die "--crossref requires a file path"
      CROSSREF_JSON="$2"
      shift 2
      ;;
    --digest)
      [[ -z "${2:-}" ]] && die "--digest requires a file path"
      DIGEST_JSON="$2"
      shift 2
      ;;
    --context)
      [[ -z "${2:-}" ]] && die "--context requires a file path"
      CONTEXT_JSON="$2"
      shift 2
      ;;
    --output-dir)
      [[ -z "${2:-}" ]] && die "--output-dir requires a path"
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --help|-h)
      usage
      ;;
    *)
      die "Unknown option: $1"
      ;;
  esac
done

# Validate required inputs
[[ -z "$WORKERS_JSON" ]] && die "--workers is required"
[[ -f "$WORKERS_JSON" ]] || die "File not found: $WORKERS_JSON"
[[ -z "$CONTEXT_JSON" ]] && die "--context is required"
[[ -f "$CONTEXT_JSON" ]] || die "File not found: $CONTEXT_JSON"

# Optional inputs — pass empty string if not provided
[[ -n "$SFC_JSON" ]] && { [[ -f "$SFC_JSON" ]] || die "File not found: $SFC_JSON"; }
[[ -n "$CROSSREF_JSON" ]] && { [[ -f "$CROSSREF_JSON" ]] || die "File not found: $CROSSREF_JSON"; }
[[ -n "$DIGEST_JSON" ]] && { [[ -f "$DIGEST_JSON" ]] || die "File not found: $DIGEST_JSON"; }

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------

if ! command -v python3 &>/dev/null; then
  die "python3 is required but not found."
fi

# Clean and create output directory
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# ---------------------------------------------------------------------------
# Worker prompt assembly (python3)
# ---------------------------------------------------------------------------

export CR_WORKERS_JSON="$WORKERS_JSON"
export CR_SFC_JSON="${SFC_JSON:-}"
export CR_CROSSREF_JSON="${CROSSREF_JSON:-}"
export CR_DIGEST_JSON="${DIGEST_JSON:-}"
export CR_CONTEXT_JSON="$CONTEXT_JSON"
export CR_OUTPUT_DIR="$OUTPUT_DIR"

python3 -c "$(cat <<'PYEOF'
import sys, json, re, os

workers_path = os.environ['CR_WORKERS_JSON']
sfc_path = os.environ.get('CR_SFC_JSON', '')
crossref_path = os.environ.get('CR_CROSSREF_JSON', '')
digest_path = os.environ.get('CR_DIGEST_JSON', '')
context_path = os.environ['CR_CONTEXT_JSON']
output_dir = os.environ['CR_OUTPUT_DIR']

# ── Load data ───────────────────────────────────────────────────────────────

with open(workers_path) as f:
    workers_data = json.load(f)

with open(context_path) as f:
    context_data = json.load(f)

sfc_data = []
if sfc_path and os.path.isfile(sfc_path):
    with open(sfc_path) as f:
        sfc_data = json.load(f)

crossref_data = []
if crossref_path and os.path.isfile(crossref_path):
    with open(crossref_path) as f:
        crossref_data = json.load(f)

digest_data = {}
if digest_path and os.path.isfile(digest_path):
    with open(digest_path) as f:
        digest_data = json.load(f)

# ── Build lookup tables ─────────────────────────────────────────────────────

# SFC sections by file path (basename match)
sfc_by_file = {}
for entry in sfc_data:
    filepath = entry.get('file', '')
    basename = os.path.basename(filepath)
    sfc_by_file[basename] = entry
    sfc_by_file[filepath] = entry

# Crossref by file path
crossref_by_file = {}
for entry in crossref_data:
    filepath = entry.get('file', '')
    basename = os.path.basename(filepath)
    crossref_by_file[basename] = entry
    crossref_by_file[filepath] = entry

# Context (diffs) by file path
context_by_file = {}
if isinstance(context_data, list):
    ctx_files = context_data
elif isinstance(context_data, dict):
    ctx_files = context_data.get('files', [])
else:
    ctx_files = []

for entry in ctx_files:
    filepath = entry.get('path', '')
    basename = os.path.basename(filepath)
    context_by_file[basename] = entry
    context_by_file[filepath] = entry

# Digest resolved/pending/dismissed
digest_resolved = set()
digest_all = []
for category in ['resolved', 'pending', 'dismissed']:
    for item in digest_data.get('digest', {}).get(category, []):
        path = item.get('path', '')
        line = item.get('line', 0)
        digest_all.append({**item, 'category': category})
        if category == 'resolved':
            digest_resolved.add((os.path.basename(path), line))

# ── Tag-to-section mapping ──────────────────────────────────────────────────

TEMPLATE_TAGS = {
    'v-if', 'v-for', 'v-show', 'v-else', 'v-model', 'v-slot',
    'aria-', 'role-', 'role=', 'tabindex', 'aria-labelledby', 'aria-live',
    'aria-hidden', 'aria-expanded', 'aria-controls', 'aria-label',
    'heading', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
    'landmark', 'section', 'nav', 'main', 'region',
    'focus-visible', 'keyboard', 'interactive', 'nested',
    'pug', 'template', 'kebab', 'tag',
    'alt', 'alt-text', 'image', 'img', 'decorative',
}

SCRIPT_TAGS = {
    'computed', 'methods', 'ref', 'reactive', 'watch',
    'data', 'props', 'emit', 'emits', 'defineProps', 'defineEmits',
    'composable', 'mixin', 'use', 'store', 'vuex', 'pinia',
    'mapState', 'mapGetters', 'mapActions', 'dispatch', 'commit',
    'optional-chaining', 'unused', 'import', 'variable', 'dead-code',
    'const', 'magic-number', 'static', 'module-constants',
    'inline', 'event', 'handler', 'extract', 'repeated', 'cache',
    'error', 'async', 'try-catch', 'domain', 'singleton', 'race',
    'concurrent', 'guard', 'return', 'function-prop', 'callback',
    'trackMREvent', 'tracking', 'segment',
}

STYLE_TAGS = {
    'scoped', 'stylus', 'scss', 'css', 'style',
    ':deep', 'deep', 'override', 'sdk',
    'margin', 'padding', 'spacing', 'flex', 'grid', 'display',
    'font', 'color', 'background', 'border', 'shadow',
    'nesting', 'alphabetized', 'utility', 'class',
    'design-system', 'variable', 'brand-color', 'ui-color', 'cta-color',
    'rem', 'em', 'px', 'border-radius',
    'max-at-tweak', 'responsive',
    'MrBtn', 'mrbtn', 'btn', 'component',
}

def get_worker_sections(worker):
    """Determine which SFC sections a worker needs based on its rules."""
    worker_id = worker.get('id', '')
    rules = worker.get('rules', [])

    # Check rule file names and paths for hints
    rule_text = ' '.join(rules).lower()

    needs_template = False
    needs_script = False
    needs_style = False

    # Category-based mapping
    cat = worker_id.lower()
    if any(k in cat for k in ['ada', 'template', 'accessibility']):
        needs_template = True
    if any(k in cat for k in ['script', 'code-style', 'code_style']):
        needs_script = True
    if any(k in cat for k in ['styling', 'style', 'mobile', 'viewport']):
        needs_style = True
    if any(k in cat for k in ['naming', 'component', 'mrbtn', 'sdk', 'third-party', 'images', 'tracking']):
        needs_template = True
        needs_script = True
        needs_style = True

    # Tag-based refinement: scan rule file names
    for tag in TEMPLATE_TAGS:
        if tag in rule_text:
            needs_template = True
            break
    for tag in SCRIPT_TAGS:
        if tag in rule_text:
            needs_script = True
            break
    for tag in STYLE_TAGS:
        if tag in rule_text:
            needs_style = True
            break

    # Fallback: if nothing matched, include all sections
    if not needs_template and not needs_script and not needs_style:
        needs_template = True
        needs_script = True
        needs_style = True

    return needs_template, needs_script, needs_style

# ── Build per-worker context ────────────────────────────────────────────────

workers = workers_data.get('workers', [])
summary = []

for worker in workers:
    worker_id = worker.get('id', 'unknown')
    rules = worker.get('rules', [])
    rule_count = worker.get('ruleCount', len(rules))
    index_md = worker.get('indexMd', worker.get('index', ''))
    category = worker.get('category', worker_id)

    needs_template, needs_script, needs_style = get_worker_sections(worker)

    # Build relevant sections from SFC data
    relevant_sections = []
    crossref_hints = []
    already_flagged = []
    diff_hunks = []

    # Get all files from context
    all_files = set()
    for entry in ctx_files:
        all_files.add(os.path.basename(entry.get('path', '')))
        all_files.add(entry.get('path', ''))

    for filepath_key in all_files:
        basename = os.path.basename(filepath_key)

        # SFC sections
        sfc_entry = sfc_by_file.get(basename) or sfc_by_file.get(filepath_key)
        if sfc_entry and sfc_entry.get('sections'):
            sections = sfc_entry['sections']
            if needs_template and sections.get('template'):
                t = sections['template']
                relevant_sections.append({
                    'file': basename,
                    'section': 'template',
                    'lang': t.get('lang', 'html'),
                    'startLine': t.get('startLine', 0),
                    'endLine': t.get('endLine', 0),
                    'content': t.get('content', ''),
                })
            if needs_script and sections.get('script'):
                s = sections['script']
                relevant_sections.append({
                    'file': basename,
                    'section': 'script',
                    'lang': s.get('lang', 'js'),
                    'setup': s.get('setup', False),
                    'startLine': s.get('startLine', 0),
                    'endLine': s.get('endLine', 0),
                    'content': s.get('content', ''),
                })
            if needs_style and sections.get('styles'):
                for st in sections['styles']:
                    relevant_sections.append({
                        'file': basename,
                        'section': 'style',
                        'lang': st.get('lang', 'css'),
                        'scoped': st.get('scoped', False),
                        'startLine': st.get('startLine', 0),
                        'endLine': st.get('endLine', 0),
                        'content': st.get('content', ''),
                    })

        # Crossref hints
        xref = crossref_by_file.get(basename) or crossref_by_file.get(filepath_key)
        if xref and xref.get('crossrefSummary'):
            cs = xref['crossrefSummary']
            if needs_script and not needs_template:
                # Script worker: tell it what template uses
                used = cs.get('usedInBoth', [])
                if used:
                    crossref_hints.append(f"{basename}: template uses [{', '.join(used[:15])}]")
            elif needs_template and not needs_script:
                # Template worker: tell it what script defines
                used = cs.get('usedInBoth', [])
                if used:
                    crossref_hints.append(f"{basename}: script defines [{', '.join(used[:15])}]")

        # Digest items for this file
        for item in digest_all:
            item_basename = os.path.basename(item.get('path', ''))
            if item_basename == basename:
                already_flagged.append({
                    'file': item_basename,
                    'line': item.get('line', 0),
                    'summary': item.get('summary', ''),
                    'status': item.get('category', 'unknown'),
                    'botAuthor': item.get('botAuthor', ''),
                })

        # Diff hunks for non-SFC files (JS, TS, etc.) or when SFC not available
        ctx_entry = context_by_file.get(basename) or context_by_file.get(filepath_key)
        if ctx_entry and not sfc_entry:
            diff_hunks.append({
                'file': ctx_entry.get('path', basename),
                'language': ctx_entry.get('language', ''),
                'diff': ctx_entry.get('diff', ''),
                'changedLineRanges': ctx_entry.get('changedLineRanges', ''),
            })

    # Deduplicate already_flagged by (file, line)
    seen_flagged = set()
    unique_flagged = []
    for item in already_flagged:
        key = (item['file'], item['line'])
        if key not in seen_flagged:
            seen_flagged.add(key)
            unique_flagged.append(item)

    # Build worker prompt JSON
    prompt_data = {
        'workerId': worker_id,
        'category': category,
        'indexMd': index_md,
        'rules': rules,
        'ruleCount': rule_count,
        'relevantSections': relevant_sections,
        'crossrefHints': crossref_hints,
        'alreadyFlagged': unique_flagged,
        'diffHunks': diff_hunks,
        'sectionScope': {
            'template': needs_template,
            'script': needs_script,
            'style': needs_style,
        },
    }

    # Write to file
    output_path = os.path.join(output_dir, f"{worker_id}.json")
    with open(output_path, 'w') as f:
        json.dump(prompt_data, f, indent=2)

    summary.append({
        'workerId': worker_id,
        'ruleCount': rule_count,
        'sectionCount': len(relevant_sections),
        'crossrefHints': len(crossref_hints),
        'alreadyFlagged': len(unique_flagged),
        'diffHunks': len(diff_hunks),
        'outputFile': output_path,
    })

# Summary to stdout
output = {
    'totalWorkers': len(summary),
    'outputDir': output_dir,
    'workers': summary,
}
print(json.dumps(output, indent=2))
PYEOF
)"
