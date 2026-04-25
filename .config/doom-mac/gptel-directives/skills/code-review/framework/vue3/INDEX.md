# framework/vue3/ — Vue 3 Core Rules

11 rules. Loaded when Vue 3 detected.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-fw-vue-001 | vif-guard-null.md | v-if guards on all sections/images where data may be null | HIGH |
| rule-fw-vue-002 | api-style-consistency.md | Maintain consistent API style (Options vs Composition) within a codebase area | MEDIUM |
| rule-fw-vue-003 | field-order.md | Follow canonical field order in Options API components | MEDIUM |
| rule-fw-vue-004 | explicit-emits.md | Declare emits option explicitly when component emits events | MEDIUM |
| rule-fw-vue-005 | computed-alphabetized.md | Alphabetize computed properties within script section | LOW |
| rule-fw-vue-006 | store-helpers-top.md | Store helpers (mapState, mapGetters, storeToRefs) at top of their section | MEDIUM |
| rule-fw-vue-007 | props-before-deep.md | Use props API to style child components before resorting to :deep() | HIGH |
| rule-fw-vue-008 | parent-prepares-data.md | Parent computes/transforms data before passing as props | MEDIUM |
| rule-fw-vue-009 | matchmedia-not-resize.md | Use window.matchMedia() not window.addEventListener('resize') | HIGH |
| rule-fw-vue-010 | template-method-trace.md | Every method called in template must trace to a definition | MEDIUM |
| rule-fw-vue-011 | function-props-async.md | Function props are valid for async callbacks that return data | MEDIUM |

**Worker instructions:** Review changed files against these rules. Report YAML. Return `NO VIOLATIONS` if clean.
