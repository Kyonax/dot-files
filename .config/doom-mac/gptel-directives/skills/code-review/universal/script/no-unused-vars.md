---
id: rule-u-sc-001
title: No Unused Variables
severity: MEDIUM
tags: unused, import, variable, dead-code
---

Remove all unused imports and variables; dead references increase bundle size and obscure intent.

### Apply
- Variables declared but never read
- Imports that are no longer referenced after a refactor
- Destructured properties that are never used

### Skip
- Type-only imports consumed exclusively in JSDoc annotations (e.g., `@typedef` or `@type`)
- Side-effect imports such as CSS files or polyfills (e.g., `import './polyfill';`)

### Bad
```javascript
import { formatDate, formatCurrency } from '@/utils/format';
import debounce from 'lodash/debounce';

const TAX_RATE = 0.08;

function getTotal(price) {
  return formatCurrency(price);
}
```

### Good
```javascript
import { formatCurrency } from '@/utils/format';

function getTotal(price) {
  return formatCurrency(price);
}
```

### Edge
Type-only JSDoc imports (`/** @typedef {import('./types').User} User */`) and side-effect imports (`import './styles.css';`, `import 'core-js/stable';`) are valid exceptions — they have no runtime reference but serve a real purpose.
