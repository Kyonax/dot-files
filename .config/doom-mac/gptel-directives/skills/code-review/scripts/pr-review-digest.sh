#!/bin/bash
# PR review comments -> resolved/pending/dismissed digest JSON
# Usage: bash scripts/pr-review-digest.sh [--pr-json <path>]
# Or:    bash scripts/pr-fetch.sh 20652 --full-body | bash scripts/pr-review-digest.sh
#
# Requires: python3
# Input:  JSON from pr-fetch.sh (with --full-body for best results; works with body_preview too)
# Output: JSON digest classifying each bot comment as resolved, pending, or dismissed

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
Usage: bash scripts/pr-review-digest.sh [--pr-json <path>]

Reads pr-fetch.sh JSON from stdin or a file and classifies each bot review
comment as resolved, pending, or dismissed based on author reply patterns.

Options:
  --pr-json <path>  Read pr-fetch.sh output from file instead of stdin
  --help            Show this help

Output: JSON to stdout with resolved[], pending[], dismissed[] arrays.
USAGE
  exit 1
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

PR_JSON_FILE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --pr-json)
      [[ -z "${2:-}" ]] && die "--pr-json requires a file path"
      PR_JSON_FILE="$2"
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

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------

if ! command -v python3 &>/dev/null; then
  die "python3 is required but not found."
fi

# ---------------------------------------------------------------------------
# Read input
# ---------------------------------------------------------------------------

if [[ -n "$PR_JSON_FILE" ]]; then
  [[ -f "$PR_JSON_FILE" ]] || die "File not found: $PR_JSON_FILE"
  INPUT=$(cat "$PR_JSON_FILE")
else
  INPUT=$(cat)
fi

if [[ -z "$INPUT" ]]; then
  die "No input provided. Pipe pr-fetch.sh output or use --pr-json."
fi

# ---------------------------------------------------------------------------
# Digest logic (python3)
# ---------------------------------------------------------------------------

echo "$INPUT" | python3 -c "
import sys, json, re

data = json.load(sys.stdin)

pr_number = data.get('pr_number', 0)
comments = data.get('existing_review_comments', [])
review_summaries = data.get('review_summaries', [])

# Bot detection patterns
BOT_PATTERNS = [
    r'.*\[bot\]$',
    r'^sentry.*',
    r'^pilkolint.*',
    r'^mrminionbot.*',
    r'^github-actions.*',
    r'^dependabot.*',
    r'^renovate.*',
    r'^codecov.*',
    r'^sonarcloud.*',
    r'^claude.*',
]

def is_bot(author):
    for pat in BOT_PATTERNS:
        if re.match(pat, author, re.IGNORECASE):
            return True
    return False

# Resolution signal patterns (case-insensitive)
RESOLVED_PATTERNS = [
    r'implement',
    r'fixed',
    r'done',
    r'resolved',
    r'\U0001f527',       # wrench emoji
    r'\u2705',           # check mark emoji
    r'\U0001f6e0',       # hammer and wrench
]

DISMISSED_PATTERNS = [
    r'skip',
    r'not applicable',
    r'n/a',
    r'won.t fix',
    r'wontfix',
    r'deferred',
    r'not a.* issue',
    r'false positive',
]

def classify_reply(body):
    if not body:
        return None
    body_lower = body.lower()
    for pat in RESOLVED_PATTERNS:
        if re.search(pat, body_lower):
            return 'resolved'
    for pat in DISMISSED_PATTERNS:
        if re.search(pat, body_lower):
            return 'dismissed'
    return None

def extract_summary(body):
    if not body:
        return ''
    # Use body field (full) or body_preview
    text = body.strip()
    # Take first line or first 80 chars
    first_line = text.split('\n')[0]
    # Strip markdown bold/links
    first_line = re.sub(r'\*\*([^*]+)\*\*', r'\1', first_line)
    first_line = re.sub(r'\[([^\]]+)\]\([^)]+\)', r'\1', first_line)
    if len(first_line) > 100:
        first_line = first_line[:100] + '...'
    return first_line

# Group comments by (path, line) to form threads
threads = {}
for c in comments:
    path = c.get('path', '')
    line = c.get('line', 0)
    key = (path, line)
    if key not in threads:
        threads[key] = []
    threads[key].append(c)

resolved = []
pending = []
dismissed = []

for (path, line), thread in threads.items():
    # Separate bot and human comments
    bot_comments = [c for c in thread if is_bot(c.get('author', ''))]
    human_comments = [c for c in thread if not is_bot(c.get('author', ''))]

    for bc in bot_comments:
        body_field = bc.get('body', bc.get('body_preview', ''))
        summary = extract_summary(body_field)
        bot_author = bc.get('author', '')

        # Check if any human replied with a resolution signal
        status = None
        signal = None
        for hc in human_comments:
            hbody = hc.get('body', hc.get('body_preview', ''))
            classification = classify_reply(hbody)
            if classification:
                status = classification
                # Extract the signal word
                signal = hbody[:60].strip()
                break

        entry = {
            'path': path,
            'line': line,
            'botAuthor': bot_author,
            'summary': summary,
        }

        if status == 'resolved':
            entry['resolvedBy'] = 'author-reply'
            entry['resolvedSignal'] = signal
            resolved.append(entry)
        elif status == 'dismissed':
            entry['dismissedSignal'] = signal
            dismissed.append(entry)
        else:
            entry['status'] = 'no-response'
            pending.append(entry)

# Parse review summaries for per-bot state
bot_summaries = []
for r in review_summaries:
    author = r.get('author', '')
    if is_bot(author):
        body_field = r.get('body', r.get('body_preview', ''))
        bot_summaries.append({
            'author': author,
            'state': r.get('state', ''),
            'summary': extract_summary(body_field),
        })

output = {
    'prNumber': pr_number,
    'totalComments': len(comments),
    'digest': {
        'resolved': resolved,
        'pending': pending,
        'dismissed': dismissed,
    },
    'botSummaries': bot_summaries,
}

print(json.dumps(output, indent=2))
"
