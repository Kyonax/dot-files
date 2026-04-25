# project/mr-backend/ — MR Backend Rules (CMS + SSR + Express)

12 rules. Loaded when project detected as mr-backend.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-pj-mrb-001 | cms-partial-deps.md | Audit CMS Partial store dependencies in serverPrefetch — populate all stores before partial renders | HIGH |
| rule-pj-mrb-002 | cms-parseurl.md | parseUrl validates segment count against Tophat urlParameterList — add optional params for Vue Router child routes | HIGH |
| rule-pj-mrb-003 | cms-route-cache.md | Parameterized routes cached in memory — refresh via Redis PARAM_ROUTES_INVALID broadcast | MEDIUM |
| rule-pj-mrb-004 | requrl-coupling.md | req.url couples CMS htmlRenderer and Vue Router SSR — NEVER rewrite, causes hydration mismatch | CRITICAL |
| rule-pj-mrb-005 | express-child-route.md | Express validation must cover Vue Router child paths with :path? optional param | HIGH |
| rule-pj-mrb-006 | componentless-parent.md | CMS pages with Vue Router children need component-less parent routes to prevent double-render | HIGH |
| rule-pj-mrb-007 | global-ssr-reg.md | Global components in CMS Partials need dual registration (client + registerGlobalsSsr.js) | HIGH |
| rule-pj-mrb-008 | css-only-responsive.md | CSS column-count/grid over JS matchMedia for SSR layouts — prevents hydration mismatch | MEDIUM |
| rule-pj-mrb-009 | ssr-timezone.md | Time-dependent values must be computed, not data() — SSR serializes UTC, client needs local timezone | MEDIUM |
| rule-pj-mrb-010 | cookie-state-xfer.md | Vuex dies on cross-app reload — use cookies for state survival across app boundaries | HIGH |
| rule-pj-mrb-011 | track-nav-select.md | Use trackMREventAndRedirect for cross-CMS-context navigation, not trackMREvent + goToPath sequentially | HIGH |
| rule-pj-mrb-012 | breadcrumb-canonical.md | Breadcrumb URLs must match canonical routes.js paths | MEDIUM |

**Worker instructions:** Review Express routes, controllers, and SSR code against MR backend conventions. Report YAML. Return `NO VIOLATIONS` if clean.
