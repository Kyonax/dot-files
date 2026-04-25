# framework/express/ — Express Server Rules

3 rules. Loaded when Express detected.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-fw-exp-001 | no-console-use-logger.md | Use the project's logger module, not console.log/console.error | MEDIUM |
| rule-fw-exp-002 | error-codes-not-html.md | Return structured error codes (JSON), not HTML error pages | MEDIUM |
| rule-fw-exp-003 | no-string-match-message.md | Never string-match response.message for control flow | MEDIUM |

**Worker instructions:** Review changed files against these rules. Report YAML. Return `NO VIOLATIONS` if clean.
