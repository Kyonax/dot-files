#!/bin/bash
# Run the writing-checks tests against the WRITING CHECKs code that config.org
# tangles into config.el: once as config.el loads it (dynamic binding), once
# with lexical binding.  Usage: run.sh [path/to/config.el]
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
config="${1:-$HOME/.config/doom/config.el}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
start=$(grep -n '^(defvar kyo/prose-max-buffer-size' "$config" | cut -d: -f1)
end=$(grep -n '"AI Clear suggestions"' "$config" | cut -d: -f1)
if [ -z "$start" ] || [ -z "$end" ]; then echo "WRITING CHECKs not found in $config" >&2; exit 2; fi
sed -n "${start},${end}p" "$config" > "$work/writing-checks.el"
{ echo ';;; -*- lexical-binding: t; -*-'; cat "$work/writing-checks.el"; } > "$work/writing-checks-lexical.el"
status=0
for module in "$work/writing-checks.el" "$work/writing-checks-lexical.el"; do
  printf '%-28s ' "$(basename "$module")"
  FAKE_CLAUDE_OUT="$work" emacs --batch -Q -l "$here/writing-checks-test.el" "$module" </dev/null > "$work/log" 2>&1
  if grep -q '^ALL PASS' "$work/log"; then
    echo "all $(grep -c '^PASS' "$work/log") checks pass"
  else
    status=1
    grep -E '^(FAIL|FAILURES)' "$work/log" || { echo "crashed:"; tail -5 "$work/log"; }
  fi
done
exit $status
