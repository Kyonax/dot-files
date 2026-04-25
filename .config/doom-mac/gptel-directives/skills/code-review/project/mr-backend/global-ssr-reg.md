---
id: rule-pj-mrb-007
title: Dual Registration for Global Components in CMS Partials
severity: HIGH
tags: global, component, ssr, registration, cms-partial
---

Global components used inside CMS Partials require dual registration — client-side (normal global registration) AND server-side via `registerGlobalsSsr.js` — or SSR renders them as unknown elements.

### Apply
- Adding a globally registered Vue component to a CMS Partial template
- SSR output contains `<component-name>` as raw HTML tags instead of rendered component markup
- Component works on client (after hydration) but not in SSR HTML source

### Skip
- Components locally registered via `components: {}` in the Partial entry component or its children
- Global components used only in full CMS pages (these go through the main app SSR pipeline which already registers globals)

### Bad
```javascript
// Only registered on the client side
// website/src/vuescripts/app.js
app.component('MrButton', MrButton);

// CMS Partial SSR does NOT load app.js globals
// → <mr-button> appears as raw HTML in SSR output
```

### Good
```javascript
// Client registration
// website/src/vuescripts/app.js
app.component('MrButton', MrButton);

// Server registration for CMS Partials
// website/src/vuescripts/registerGlobalsSsr.js
export function registerGlobalsSsr(app) {
  app.component('MrButton', MrButton);
}

// Both registrations must list the same components
```

### Edge
The CMS Partial SSR pipeline creates its own Vue app instance separate from the main app. It calls `registerGlobalsSsr()` to register global components. If a component is missing from `registerGlobalsSsr.js`, SSR silently renders it as an unknown element (no error thrown). The client then hydrates and replaces it, causing a visible flash. Always check `registerGlobalsSsr.js` when adding new global components that may appear in Partials.
