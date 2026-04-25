# Brand Detection

Determines which brand rule directory to load (if any).

## Signals (in priority order)

### 1. Git Remote URL
```bash
git remote get-url origin 2>/dev/null
```
| Pattern | Brand | Load |
|---|---|---|
| `Kyonax/*` or `kyonax/*` | Kyonax | `brand/kyonax/` |
| `MadisonReed/*` | Madison Reed | No brand dir (project detection handles specifics) |
| Anything else | Generic | No brand dir |

### 2. Explicit User Override
User says "apply Kyonax rules" or "use MR rules" → honor regardless of remote.

### 3. Repo-Local Indicators (when remote is ambiguous)
| Indicator | Brand |
|---|---|
| `@<brand>/` folders in root | Kyonax |
| `sources.js` with `type: 'hud'\|'animation'\|'scene'` | Kyonax |
| `use-obs-websocket.js` imports | Kyonax |
| `CHANGELOG.org` with Tier 1 headers | Kyonax |
| `website/src/vuescripts/` path exists | Madison Reed |
| `mr_modules/` path exists | Madison Reed |

## Output
```json
{ "brand": "kyonax" | "madison-reed" | "generic" }
```

## Precedence
Brand and project/tech-stack rules are **orthogonal** — both load when matched. Brand doesn't replace project.
