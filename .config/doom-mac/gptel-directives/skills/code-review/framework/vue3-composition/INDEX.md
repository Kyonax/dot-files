# framework/vue3-composition/ — Vue 3 Composition API Rules

3 rules. Loaded when Composition API or script setup detected.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-fw-vca-001 | script-setup-conventions.md | Order in script setup: imports, store, props/emits, refs, computed, methods, lifecycle | MEDIUM |
| rule-fw-vca-002 | composable-over-mixin.md | Use composables for shared logic, never create new mixins | HIGH |
| rule-fw-vca-003 | ref-reactive-patterns.md | Use ref() for primitives, reactive() for objects, no double-wrapping | MEDIUM |

**Worker instructions:** Review changed files against these rules. Report YAML. Return `NO VIOLATIONS` if clean.
