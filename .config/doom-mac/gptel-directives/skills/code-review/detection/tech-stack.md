# Tech-Stack Detection

Determines which framework rule directories to load.

## Signals

### 1. package.json Dependencies
```bash
cat package.json | grep -o '"vue"\|"react"\|"express"\|"next"\|"nuxt"\|"@vue/composition-api"'
```

| Dependency | Framework | Load |
|---|---|---|
| `vue` (3.x) | Vue 3 | `framework/vue3/` |
| `vue` (2.x) | Vue 2 | `framework/vue3/` (subset applies) |
| `@vue/composition-api` or `<script setup>` in files | Vue 3 Composition API | `framework/vue3-composition/` (additive to vue3/) |
| `react` | React | (no framework rules yet — universal only) |
| `express` | Express | `framework/express/` |
| `next` | Next.js | (no framework rules yet — universal only) |
| `nuxt` | Nuxt | `framework/vue3/` + `framework/vue3-composition/` |

### 2. File Extension Distribution
```bash
find . -name "*.vue" | head -1    # Vue detected
find . -name "*.jsx" -o -name "*.tsx" | head -1   # React detected
```

### 3. Config File Detection
| File | Framework |
|---|---|
| `vite.config.*` | Vite (likely Vue or React) |
| `webpack.config.*` | Webpack (check package.json for framework) |
| `tsconfig.json` | TypeScript (modifier, not framework) |
| `nuxt.config.*` | Nuxt (Vue 3 + Composition) |
| `next.config.*` | Next.js (React) |

### 4. Composition API Detection
If Vue 3 detected, check if changed files use Composition API:
```bash
grep -l "<script setup>\|import { ref\|import { computed\|useStore\|defineProps\|defineEmits" <changed-files>
```
If any match → also load `framework/vue3-composition/`.

## Multiple Frameworks
A monorepo may have both Vue and Express. Load ALL matched framework directories.

## Output
```json
{ "techStack": ["vue3", "vue3-composition", "express"] }
```

## Fallback
No framework detected → load only `universal/` rules. Every project gets universal rules regardless.
