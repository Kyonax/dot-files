# Project Detection

Determines which project rule directory to load based on changed file paths.

## Signals

Match changed file paths against these patterns (first match wins):

| Path Pattern | Project | Load |
|---|---|---|
| `website/src/vuescripts/` | `mr-dotcom` | `project/mr-dotcom/` (all subdirs) |
| `website/src/routing/` | `mr-dotcom` | `project/mr-dotcom/` |
| `website/src/views/` | `mr-dotcom` | `project/mr-dotcom/` |
| `mr_modules/` | `mr-backend` | `project/mr-backend/` |
| `website/src/madison.js` | `mr-backend` | `project/mr-backend/` |
| `tophat/` | `mr-tophat` | (no project rules yet) |
| `raven/` | `mr-raven` | (no project rules yet) |
| `@<brand>/sources/hud/` | `kyonax-obs-hud` | `project/kyonax-obs-hud/` (inherits brand/kyonax/) |
| `src/shared/composables/use-obs-*` | `kyonax-obs-hud` | `project/kyonax-obs-hud/` |
| No match | `generic` | No project dir |

## Multi-Project PRs

When changed files span multiple projects (e.g., `website/src/vuescripts/` AND `mr_modules/`), load ALL matched project directories. Workers handle the union.

## Output
```json
{ "project": "mr-dotcom" | "mr-backend" | "kyonax-obs-hud" | "generic" }
```
Or for multi-project:
```json
{ "project": ["mr-dotcom", "mr-backend"] }
```
