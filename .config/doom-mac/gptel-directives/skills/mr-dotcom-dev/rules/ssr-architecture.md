---
title: SSR Architecture — Vite, Vue Server Rendering, and Hydration
impact: HIGH
impactDescription: Defines the SSR rendering pipeline from Express through Vite to Vue hydration, critical for understanding how pages are served and debugged.
tags: ssr, vite, hydration, server-rendering, renderToString, entry-server, entry-client, vueSsr, madison, express, hmr, client-manifest, preload
---

## SSR Architecture — Vite, Vue Server Rendering, and Hydration

The MR website uses Vue 3 Server-Side Rendering with Vite as the build tool. Pages are rendered on the server for SEO and performance, then hydrated on the client for SPA interactivity.

---

### 1. Request Flow

```
Browser Request
    ↓
Express (madison.js)
    ↓
Middleware (morgan, bodyParser, compression, helmet)
    ↓
before.js (query param processing, promo codes)
    ↓
Route handler (views.js)
    ├─ Legacy: res.render('template.pug', data)
    └─ Vue SSR:
        ↓
    mr_modules/cms/lib/vueSsr.js
        ↓
    getServerRenderer() → appRender(ssrContext)
        ↓
    ssr/entry/server.js → createApp() → renderToString()
        ↓
    Extract: HTML, Vuex state, CSS, preload links
        ↓
    Inject into Pug template → Send to browser
        ↓
Client receives HTML
    ↓
    ssr/entry/client.js
        ↓
    Restore Vuex state from window.__INITIAL_STATE__
        ↓
    Register client-only modules (notifications, emailCapture)
        ↓
    app.mount('#mrVueApp') → Hydration complete
```

---

### 2. Key Files

| File | Purpose |
|---|---|
| `website/src/madison.js` | Express server entry point |
| `mr_modules/cms/lib/vueSsr.js` | SSR renderer factory (dev + prod modes) |
| `website/src/vuescripts/ssr/ssrApp.js` | Vue app factory for SSR |
| `website/src/vuescripts/ssr/entry/server.js` | Server entry — creates app, provides SSR context |
| `website/src/vuescripts/ssr/entry/client.js` | Client entry — hydrates app, restores Vuex state |
| `website/vite/vite.dev.config.mjs` | Vite dev config (includes test config) |
| `website/vite/vite.common.config.mjs` | Shared Vite config (aliases, plugins) |
| `website/vite/vite.client.config.mjs` | Client build config |
| `website/vite/vite.ssr.server.config.mjs` | SSR server build config |

---

### 3. Dual-Mode Renderer (`vueSsr.js`)

**Development mode:**
- Vite runs in **middleware mode** with HMR
- The dev server is attached to Express: `app.use(vite.middlewares)`
- Config: `website/vite/vite.dev.config.mjs`
- Server entry loaded via `vite.ssrLoadModule()`
- Changes to Vue files hot-reload instantly

**Production mode:**
- Pre-built server bundle: `website/lib/server.js`
- Client manifest: `website/src/public/scripts/vueBundle/ssr/vue-ssr-client-manifest.json`
- Manifest drives preload link generation (fonts, CSS, JS chunks)
- CSS extracted from Vite modules, limited to 10 concurrent operations

---

### 4. Server Entry (`entry/server.js`)

```javascript
export default async function createServerApp(context) {
  const { html, computedContentClasses, cmsContentId } = context;
  const { app, store, router } = await createApp({
    html, computedContentClasses, cmsContentId
  });

  app.provide('$ssrContext', context);
  await router.replace(context?.url?.path || "/");
  await router.isReady();

  return { app, getStore() { return store; } };
}
```

The server creates the Vue app, navigates the router, and returns the app + store for rendering and state serialization.

---

### 5. Client Entry (`entry/client.js`)

```javascript
async function init() {
  const { app, store, router } = await createApp({
    html: window.__SSR_APP_HTML__,
    computedContentClasses: window.__SSR_COMPUTED_CONTENT_CLASSES__,
    cmsContentId: window.__SSR_CMS_CONTENT_ID__,
  });

  // Hydrate Vuex state from server
  if (window.__INITIAL_STATE__) {
    store.replaceState(window.__INITIAL_STATE__);
  }

  // Register client-only store modules
  store.registerModule('notifications', notifications);
  store.registerModule('emailCapture', emailCapture);

  await router.isReady();
  app.mount('#mrVueApp');
}
```

**Hydration variables injected into HTML by the server:**
- `window.__SSR_APP_HTML__`
- `window.__SSR_COMPUTED_CONTENT_CLASSES__`
- `window.__SSR_CMS_CONTENT_ID__`
- `window.__INITIAL_STATE__` — serialized Vuex store state

---

### 6. Vite Configuration Highlights

**Path aliases** (defined in `vite.common.config.mjs`, used everywhere):

| Alias | Path |
|---|---|
| `@components` | `src/vuescripts/components` |
| `@services` | `src/vuescripts/services` |
| `@store` | `src/vuescripts/store` |
| `@utilities` | `src/vuescripts/utilities` |
| `@directives` | `src/vuescripts/directives` |
| `@mrModules` | `../../mr_modules` |

**Stylus auto-injection:**
```javascript
css: {
  preprocessorOptions: {
    stylus: {
      additionalData: `@import "variables.styl";\n@import "mixins.styl";`
    }
  }
}
```

**Custom elements** (Pug tags like `<strike>`, `<font>` registered as custom Vue elements to prevent warnings).

---

### 7. Development Commands

```bash
npm run dev          # Website dev server (SSR + Vite HMR)
npm run dev-ssr      # Website with SSR only
```

**Dev mode runs:** Vite dev server in middleware mode → Express handles routes → Vite handles HMR and module transforms.
