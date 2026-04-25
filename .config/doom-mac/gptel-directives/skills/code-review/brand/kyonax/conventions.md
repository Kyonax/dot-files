---
id: rule-br-kyo-021
title: Kyonax Naming and Project Conventions
severity: MEDIUM
tags: kebab, emit, alias, colors-scss, no-git-write, convention
---

Follow Kyonax project conventions: kebab-case emits, snake_case composable state, kebab-case filenames, Vite aliases only, colors in SCSS, no git-write commands in review.

### Apply
- All Kyonax/RECKIT/OBS project code.

### Skip
- Third-party library code or generated files.

### Conventions

| Category | Convention | Example |
|---|---|---|
| Emits | kebab-case | `emit('levels-update')` not `emit('levelsUpdate')` |
| Composable state | snake_case | `let shared_state = null` not `let sharedState` |
| Filenames | kebab-case | `obs-audio-meter.vue` not `ObsAudioMeter.vue` |
| Imports | Vite aliases only | `import { x } from '@/composables/obs'` not `import { x } from '../../composables/obs'` |
| Colors | SCSS variables/maps | `color: $hud-accent` not `color: '#00ff88'` in JS |
| Code review | No git-write commands | Never run `git commit`, `git push`, `git checkout`, `git reset` during review |

### Bad
```javascript
// camelCase emit, relative import, hardcoded color in JS
import { useObsAudio } from '../../composables/obs';
emit('levelsUpdate');
el.style.color = '#00ff88';
```

### Good
```javascript
// kebab emit, alias import, color from SCSS
import { useObsAudio } from '@/composables/obs';
emit('levels-update');
// Color applied via CSS class, defined in SCSS
```

### Edge
Vue component PascalCase names in `<script>` registration are acceptable per Vue convention, but the filename on disk must still be kebab-case.
