#!/bin/bash
# Pull PR metadata + diff + existing reviews -> JSON to stdout
# Usage: bash scripts/pr-fetch.sh <pr-number> [--repo owner/repo]
#
# Requires: gh CLI installed and authenticated
# Output: JSON object with PR metadata, diff path, reviews, and CI checks

set -euo pipefail

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

die() {
  echo "ERROR: $*" >&2
  exit 1
}

require_gh() {
  if ! command -v gh &>/dev/null; then
    die "gh CLI is not installed. Install it from https://cli.github.com/"
  fi
  if ! gh auth status &>/dev/null 2>&1; then
    die "gh CLI is not authenticated. Run 'gh auth login' first."
  fi
}

usage() {
  cat <<'USAGE'
Usage: bash scripts/pr-fetch.sh <pr-number> [--repo owner/repo] [--full-body]

Arguments:
  <pr-number>       The pull request number to fetch (required)
  --repo owner/repo Override the repository (optional; auto-detected if omitted)
  --full-body       Output full comment/review bodies instead of 120-char previews

Output:
  JSON to stdout with PR metadata, diff file path, existing reviews, and CI checks.
USAGE
  exit 1
}

# json_escape: make a string safe to embed as a JSON string value.
# Handles backslashes, double quotes, newlines, tabs, and carriage returns.
json_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="${s//$'\n'/\\n}"
  s="${s//$'\t'/\\t}"
  s="${s//$'\r'/\\r}"
  printf '%s' "$s"
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

PR_NUMBER=""
REPO=""
FULL_BODY=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo)
      [[ -z "${2:-}" ]] && die "--repo requires a value (owner/repo)"
      REPO="$2"
      shift 2
      ;;
    --full-body)
      FULL_BODY=true
      shift
      ;;
    --help|-h)
      usage
      ;;
    *)
      if [[ -z "$PR_NUMBER" ]]; then
        PR_NUMBER="$1"
        shift
      else
        die "Unexpected argument: $1"
      fi
      ;;
  esac
done

[[ -z "$PR_NUMBER" ]] && usage

# Validate PR number is numeric
if ! [[ "$PR_NUMBER" =~ ^[0-9]+$ ]]; then
  die "PR number must be a positive integer, got: $PR_NUMBER"
fi

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------

require_gh

# Detect repo if not provided
if [[ -z "$REPO" ]]; then
  REPO=$(gh repo view --json nameWithOwner -q '.nameWithOwner' 2>/dev/null) || true
  if [[ -z "$REPO" ]]; then
    die "Could not detect repository. Use --repo owner/repo or run from inside a git repo."
  fi
fi

OWNER="${REPO%%/*}"
REPO_NAME="${REPO##*/}"

# ---------------------------------------------------------------------------
# 1. Fetch PR metadata
# ---------------------------------------------------------------------------

PR_JSON=$(gh pr view "$PR_NUMBER" \
  --repo "$REPO" \
  --json title,author,baseRefName,headRefName,state,labels,reviewRequests,additions,deletions,changedFiles 2>/dev/null) \
  || die "PR #$PR_NUMBER not found in $REPO (or access denied)."

PR_TITLE=$(echo "$PR_JSON" | gh pr view "$PR_NUMBER" --repo "$REPO" --json title -q '.title' 2>/dev/null || echo "")
PR_AUTHOR=$(echo "$PR_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('author',{}).get('login',''))" 2>/dev/null || echo "")
PR_BASE=$(echo "$PR_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('baseRefName',''))" 2>/dev/null || echo "")
PR_HEAD=$(echo "$PR_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('headRefName',''))" 2>/dev/null || echo "")
PR_STATE=$(echo "$PR_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('state',''))" 2>/dev/null || echo "")
PR_ADDITIONS=$(echo "$PR_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('additions',0))" 2>/dev/null || echo "0")
PR_DELETIONS=$(echo "$PR_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('deletions',0))" 2>/dev/null || echo "0")
PR_CHANGED_FILES=$(echo "$PR_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin).get('changedFiles',0))" 2>/dev/null || echo "0")

# Labels as JSON array
PR_LABELS=$(echo "$PR_JSON" | python3 -c "
import sys, json
data = json.load(sys.stdin)
labels = [l.get('name','') for l in data.get('labels', [])]
print(json.dumps(labels))
" 2>/dev/null || echo "[]")

# Review requests as JSON array
PR_REVIEW_REQUESTS=$(echo "$PR_JSON" | python3 -c "
import sys, json
data = json.load(sys.stdin)
reqs = []
for r in data.get('reviewRequests', []):
    if 'login' in r:
        reqs.append(r['login'])
    elif 'name' in r:
        reqs.append(r['name'])
print(json.dumps(reqs))
" 2>/dev/null || echo "[]")

# ---------------------------------------------------------------------------
# 2. Fetch PR diff -> save to /tmp
# ---------------------------------------------------------------------------

DIFF_DIR="/tmp/cr-pr-diff"
mkdir -p "$DIFF_DIR"
DIFF_FILE="$DIFF_DIR/pr-${PR_NUMBER}.diff"

gh pr diff "$PR_NUMBER" --repo "$REPO" > "$DIFF_FILE" 2>/dev/null \
  || die "Failed to fetch diff for PR #$PR_NUMBER."

# Extract list of changed files from the diff
FILES_ARRAY=$(grep -E '^\+\+\+ b/' "$DIFF_FILE" 2>/dev/null \
  | sed 's|^+++ b/||' \
  | python3 -c "
import sys, json
files = [line.strip() for line in sys.stdin if line.strip()]
print(json.dumps(files))
" 2>/dev/null || echo "[]")

# ---------------------------------------------------------------------------
# 3. Fetch existing review comments
# ---------------------------------------------------------------------------

REVIEW_COMMENTS=$(gh api "repos/${OWNER}/${REPO_NAME}/pulls/${PR_NUMBER}/comments" \
  --paginate 2>/dev/null \
  | python3 -c "
import sys, json

raw = sys.stdin.read().strip()
# gh --paginate may concatenate multiple JSON arrays; merge them
if raw.startswith('['):
    # Handle concatenated arrays: '][' -> '],['
    raw = raw.replace('][', '],[')
    if ',[' in raw:
        raw = '[' + raw + ']'
        items = []
        for arr in json.loads(raw):
            items.extend(arr)
    else:
        items = json.loads(raw)
else:
    items = []

full_body = '$FULL_BODY' == 'true'
comments = []
for c in items:
    body = c.get('body', '')
    entry = {
        'author': c.get('user', {}).get('login', ''),
        'path': c.get('path', ''),
        'line': c.get('line') or c.get('original_line') or 0,
    }
    if full_body:
        entry['body'] = body
    else:
        entry['body_preview'] = body[:120] + '...' if len(body) > 120 else body
    comments.append(entry)
print(json.dumps(comments))
" 2>/dev/null || echo "[]")

# ---------------------------------------------------------------------------
# 4. Fetch review summaries
# ---------------------------------------------------------------------------

REVIEW_SUMMARIES=$(gh api "repos/${OWNER}/${REPO_NAME}/pulls/${PR_NUMBER}/reviews" \
  --paginate 2>/dev/null \
  | python3 -c "
import sys, json

raw = sys.stdin.read().strip()
if raw.startswith('['):
    raw = raw.replace('][', '],[')
    if ',[' in raw:
        raw = '[' + raw + ']'
        items = []
        for arr in json.loads(raw):
            items.extend(arr)
    else:
        items = json.loads(raw)
else:
    items = []

full_body = '$FULL_BODY' == 'true'
reviews = []
for r in items:
    body = r.get('body', '')
    entry = {
        'author': r.get('user', {}).get('login', ''),
        'state': r.get('state', ''),
    }
    if full_body:
        entry['body'] = body
    else:
        entry['body_preview'] = (body[:120] + '...') if len(body) > 120 else body
    reviews.append(entry)
print(json.dumps(reviews))
" 2>/dev/null || echo "[]")

# ---------------------------------------------------------------------------
# 5. Fetch CI check status
# ---------------------------------------------------------------------------

CI_CHECKS=$(gh pr checks "$PR_NUMBER" --repo "$REPO" --json name,state,conclusion 2>/dev/null || echo "[]")

# Validate it is actual JSON; fall back to empty array
if ! echo "$CI_CHECKS" | python3 -c "import sys,json; json.load(sys.stdin)" &>/dev/null; then
  CI_CHECKS="[]"
fi

# ---------------------------------------------------------------------------
# 6. Assemble final JSON output
# ---------------------------------------------------------------------------

python3 -c "
import json, sys

output = {
    'pr_number': int(sys.argv[1]),
    'repo': sys.argv[2],
    'title': sys.argv[3],
    'author': sys.argv[4],
    'base': sys.argv[5],
    'head': sys.argv[6],
    'state': sys.argv[7],
    'labels': json.loads(sys.argv[8]),
    'review_requests': json.loads(sys.argv[9]),
    'files_changed': int(sys.argv[10]),
    'additions': int(sys.argv[11]),
    'deletions': int(sys.argv[12]),
    'files': json.loads(sys.argv[13]),
    'diff_file': sys.argv[14],
    'existing_review_comments': json.loads(sys.argv[15]),
    'review_summaries': json.loads(sys.argv[16]),
    'ci_checks': json.loads(sys.argv[17])
}

print(json.dumps(output, indent=2))
" \
  "$PR_NUMBER" \
  "$REPO" \
  "$PR_TITLE" \
  "$PR_AUTHOR" \
  "$PR_BASE" \
  "$PR_HEAD" \
  "$PR_STATE" \
  "$PR_LABELS" \
  "$PR_REVIEW_REQUESTS" \
  "$PR_CHANGED_FILES" \
  "$PR_ADDITIONS" \
  "$PR_DELETIONS" \
  "$FILES_ARRAY" \
  "$DIFF_FILE" \
  "$REVIEW_COMMENTS" \
  "$REVIEW_SUMMARIES" \
  "$CI_CHECKS"
