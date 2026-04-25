---
id: rule-pj-mrb-010
title: Use Cookies for Cross-App State Survival
severity: HIGH
tags: cookie, state, cross-app, vuex, reload
---

Vuex state dies on cross-app page reload — use cookies for state that must survive navigation across CMS app boundaries (e.g., website to Tophat, different CMS contexts).

### Apply
- User action in one CMS context must persist data visible in a different CMS context
- State must survive a full page reload (not a Vue Router client-side navigation)
- Debugging "state lost" issues where Vuex data disappears after navigation

### Skip
- State shared within the same CMS page or single-page Vue Router navigation
- Server-only state managed by session or database (not client-side)
- Short-lived UI state (modal open/closed, form validation) that does not need persistence

### Bad
```javascript
// Vuex action stores critical cross-app state only in the store
setSelectedLocation({ commit }, location) {
  commit('SET_SELECTED_LOCATION', location);
  // User navigates to a different CMS app → Vuex resets → state lost
},
```

### Good
```javascript
// Write to both Vuex (for reactivity) and cookie (for persistence)
setSelectedLocation({ commit }, location) {
  commit('SET_SELECTED_LOCATION', location);
  document.cookie = `selectedLocation=${encodeURIComponent(
    JSON.stringify(location)
  )}; path=/; max-age=86400; SameSite=Lax`;
},

// On app init, hydrate Vuex from cookie
loadSelectedLocation({ commit }) {
  const cookie = getCookie('selectedLocation');
  if (cookie) {
    commit('SET_SELECTED_LOCATION', JSON.parse(decodeURIComponent(cookie)));
  }
},
```

### Edge
Different CMS pages may run entirely separate Vue app instances with separate Vuex stores. Navigation between them triggers a full page load, destroying all client-side state. Cookies are the only reliable client-side persistence mechanism that survives this boundary. Keep cookie payloads small (under 4KB) and use `JSON.stringify` for structured data. For larger state, consider writing to `localStorage` and reading back on the other side — but cookies are preferred because they are also available during SSR via `req.cookies`.
