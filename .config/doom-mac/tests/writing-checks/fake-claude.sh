#!/bin/bash
# Stands in for the Claude Code CLI in writing-checks-test.el: records how it
# was called in $FAKE_CLAUDE_OUT (else next to itself), then answers as
# FAKE_CLAUDE_MODE says: ok (default), error, slow or linger.
dir="$(dirname "$0")"
out="${FAKE_CLAUDE_OUT:-$dir}"
printf '%s\n' "$@" > "$out/fake-claude.args"
pwd > "$out/fake-claude.cwd"
env | grep -c '^CLAUDE_CONFIG_DIR=' > "$out/fake-claude.env"
cat > "$out/fake-claude.stdin"
case "$FAKE_CLAUDE_MODE" in
  error) echo '{"type":"result","subtype":"error_during_execution","is_error":true,"result":"Not logged in · Please run /login"}'; exit 1 ;;
  slow) exec sleep 5 ;;
  linger) cat "$dir/fake-claude.answer"; echo; exec sleep 3 ;;
  *) cat "$dir/fake-claude.answer" ;;
esac
