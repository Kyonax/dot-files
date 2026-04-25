---
title: CMS + SSR + Express Routing — parseUrl Validation, req.url Coupling, Child Route Coverage
impact: HIGH
impactDescription: CMS-served pages with Vue Router children silently 404 on direct URL access, produce hydration mismatches when req.url is rewritten, or render blank shells when Express validation is bypassed. These failures only manifest on direct page access (bookmarks, shared links, refresh) — never during SPA navigation.
tags: cms, ssr, express, routing, parseUrl, urlParameterList, tophat, parameterized, route-cache, initRoutes, redis, PARAM_ROUTES_INVALID, req-url, hydration, mismatch, htmlRenderer, ssrContext, server-entry, component-less, parent-route, children, path-optional, serverPrefetch, cross-store, bridge, cookie, state-transfer, trackMREventAndRedirect, breadcrumb, canonical, global-registration, registerGlobalsSsr
---

Rules for reviewing Express routes, CMS URL resolution, and Vue Router SSR interactions in the MR monorepo. The CMS pipeline (`parseUrl` → `loadPage` → `htmlRenderer` → `renderToString`) and Vue Router SSR share `req.url` — a coupling that constrains how child routes, URL rewriting, and Express middleware can work. These 12 rules address the most dangerous failure modes: silent 404s, hydration mismatches, blank page shells, and cross-app state loss.

## Rules

| # | Rule | Severity | Section |
|---|---|---|---|
| 1 | **cms-partial-deps** — Audit CMS Partial store dependencies in serverPrefetch | HIGH | [Details](#cms-partial-deps) |
| 2 | **cms-parseurl** — CMS parseUrl validates segment count against Tophat urlParameterList | HIGH | [Details](#cms-parseurl) |
| 3 | **cms-route-cache** — Parameterized routes cached in memory; refresh via Redis broadcast | MEDIUM | [Details](#cms-route-cache) |
| 4 | **requrl-coupling** — req.url coupled between CMS and Vue Router SSR — never rewrite | CRITICAL | [Details](#requrl-coupling) |
| 5 | **express-child-route** — Express validation must cover Vue Router child paths with :path? | HIGH | [Details](#express-child-route) |
| 6 | **componentless-parent** — CMS pages with Vue Router children need component-less parent routes | HIGH | [Details](#componentless-parent) |
| 7 | **global-ssr-reg** — Global components in CMS Partials need dual registration | HIGH | [Details](#global-ssr-reg) |
| 8 | **css-only-responsive** — CSS column-count/grid over JS matchMedia for SSR layouts | MEDIUM | [Details](#css-only-responsive) |
| 9 | **ssr-timezone** — Time-dependent values must be computed, not data() | MEDIUM | [Details](#ssr-timezone) |
| 10 | **cookie-state-xfer** — Vuex dies on cross-app reload; use cookies | HIGH | [Details](#cookie-state-xfer) |
| 11 | **track-nav-select** — trackMREventAndRedirect for cross-CMS-context navigation | HIGH | [Details](#track-nav-select) |
| 12 | **breadcrumb-canonical** — Breadcrumb URLs must match canonical routes.js paths | MEDIUM | [Details](#breadcrumb-canonical) |

---

## cms-partial-deps

When a CMS Partial (`CMSPartial`/`CmsPartialSsr`) references globally registered components that read Vuex store state, the page's `serverPrefetch` MUST populate those stores BEFORE the partial renders. CMS Partials fail silently — no error, just empty markup.

**Incorrect:**
```javascript
async serverPrefetch() {
  await this.loadLocation(code); // colorbar store only
  this.setBookingLocation(this.location); // shallow copy — no servicesOffered
  // FeaturedServicesV2 reads hairColorBarBooking.location.servicesOffered → undefined → renders nothing
}
```

**Correct:**
```javascript
async serverPrefetch() {
  await Promise.all([
    this.loadLocation(code),           // colorbar store
    this.getBookingLocation(code),     // hairColorBarBooking store — populates servicesOffered
  ]);
}
```

**Exception:** Experiment-gated components (behind `v-if="isExperimentB"`) skip SSR entirely — `serverPrefetch` bridging is irrelevant for them.

**Validated:** DOTCOMPB-7742 — `FeaturedServicesV2` silently rendered nothing. Two different API endpoints return different data shapes; only the booking endpoint returns `servicesOffered`.

---

## cms-parseurl

CMS `parseUrl` (`router.js:650`) validates incoming URL segments against `urlParameterList` from MongoDB. If the URL has more segments than parameters allow, `parseUrl` returns the raw URL → `loadPage` exact-match fails → 404. When adding Vue Router child routes, Tophat must define optional parameters for each child path segment.

**Verify via MongoDB:**
```bash
docker exec mr-mongo mongosh --quiet --eval \
  'db.getSiblingDB("cms").content.findOne({ _id: N }, { uri: 1, urlParameterList: 1 })'
```

**Example:** `/colorbar/locations/peachtreecorners/photos` = 2 segments. If Tophat only has `locationCode` (1 param), validation fails. Fix: add `{ name: 'photos', optional: true }` to `urlParameterList`.

**Exception:** Non-CMS pages using `res.render()` bypass `parseUrl` entirely.

---

## cms-route-cache

CMS parameterized routes are cached in memory via `initRoutes()` at startup. After Tophat URL parameter changes, refresh: (a) re-save content in Tophat (triggers Redis `PARAM_ROUTES_INVALID` broadcast), (b) restart server, or (c) manual: `redis-cli publish broadcast '{"event":"PARAM_ROUTES_INVALID","data":{"env":"DEV"}}'`.

**Edge case:** If Redis is not running (local dev without Redis container), broadcasts have no effect — server restart is the only option.

---

## requrl-coupling

`req.url` flows through: `htmlRenderer.js:536` → `locals.parsedUrl` → `ssrContext.url` → `server.js:22` `router.replace(context.url.path)`. CMS uses `req.url` for page resolution. Vue Router SSR uses the SAME `req.url` for route navigation. NEVER rewrite `req.url` — the server renders the wrong child route → hydration mismatch.

**Incorrect:**
```javascript
// Rewrite URL for CMS resolution
app.get('/locations/:urlKey/photos', (req, res, next) => {
  req.url = `/locations/${req.params.urlKey}`; // SSR renders location-details, client expects location-photos
  next();
});
```

**Correct:** Make CMS accept the full URL via Tophat `urlParameterList`. Preserve `req.url` intact.

**Exception:** Non-CMS pages (direct `res.render()`) bypass `htmlRenderer.js`.

---

## express-child-route

Express `:param` only matches single path segments. `/peachtreecorners/photos` skips a handler for `/:urlKey`. Use `:path?` optional param so the existing validation handler covers child routes.

**Incorrect:**
```javascript
app.get('/colorbar/locations/:urlKey', validateLocation); // skips /urlKey/photos
```

**Correct:**
```javascript
app.get('/colorbar/locations/:urlKey/:path?', validateLocation); // covers any child
```

Handler body only uses `req.params.urlKey` — `:path` is ignored. Without coverage, invalid codes on child paths return 200 blank shell instead of 404.

**Exception:** Client-only SPA navigation (`$router.push`) never hits Express.

**Validated:** DOTCOMPB-7712 — `/locations/invalid/photos` returned 200 blank. 4 code review rounds to diagnose.

---

## componentless-parent

CMS-rendered pages with Vue Router children must use component-less parent routes (no `component`, just `path` + `children`). Setting the CMS page component as the route `component` causes double-render: CMS renders the outer instance (with `cmsSettings`), `router-view` renders an inner instance (without `cmsSettings`).

**Incorrect:**
```javascript
{ path: '/locations/:code', component: HcbLocationPageV2, children: [...] } // double-render
```

**Correct:**
```javascript
{ path: '/locations/:code', children: [
  { path: '', name: 'location-details', component: HcbLocationSections },
  { path: 'photos', name: 'location-photos', component: HcbLocationPhotosPage },
]}
```

**Exception:** When parent and child are DIFFERENT components (Dashboard + EditAutoDelivery), normal nesting works.

---

## global-ssr-reg

Globally registered components used in CMS Partial HTML must be registered in BOTH `mrVueApp.js` (client) AND `registerGlobalsSsr.js` (SSR via `defineAsyncComponent`). Missing SSR registration = silent empty markup during SSR.

**Exception:** Route components (imported by `routes.js`) don't need global registration. Experiment-B-only components that never render during SSR don't need SSR registration.

---

## css-only-responsive

Prefer CSS `column-count`/grid/media queries over JS `matchMedia` for SSR-rendered layouts. JS `matchMedia` causes hydration mismatch: server renders default (no `window`), client detects actual viewport.

**Exception:** `matchMedia` is acceptable for binary show/hide (`v-if`) that swaps entirely different component trees — the flash is expected and accepted.

**Validated:** DOTCOMPB-7712 — photos masonry changed from JS `matchMedia` (2 cols SSR, 4 client → mismatch) to CSS `column-count` (zero mismatch).

---

## ssr-timezone

Time-dependent values (`new Date().getDay()`) must be `computed`, not `data()`. `data()` runs once during SSR (server TZ, e.g., UTC) and serializes. Client reads the serialized value instead of recomputing for the user's local TZ.

**Exception:** Client-only components (experiment-gated) are exempt — `data()` only runs on client.

---

## cookie-state-xfer

Vuex state dies on cross-app page reloads. Use cookies for state that must survive. Set cookie before `trackMREventAndRedirect`. Consume in target page's `mounted()`.

**Incorrect:**
```javascript
this.setSelectedService(service); // Vuex — lost on page reload
this.$router.push({ name: 'booking-calendar' });
```

**Correct:**
```javascript
this.$cookies.set('selected_service', service.code);
this.trackMREventAndRedirect('event', `/colorbar/booking/${code}/services`, props);
```

**Exception:** Within the same SPA entry point (no page reload), Vuex + `$router.push` works.

---

## track-nav-select

`trackMREvent` = stays on page. `trackMREventAndRedirect` = hard redirect (300ms delay for Segment flush). NEVER `trackMREvent()` + `goToPath()` sequentially — event may not flush. Use `trackMREventAndRedirect` for cross-CMS-context navigation. Use `$router.push` within the SAME CMS page.

**Exception:** `<a>` with native `href` + `target="_blank"` — use `trackMREvent` (fire-and-forget, new tab doesn't kill current page).

---

## breadcrumb-canonical

Breadcrumb URLs must match canonical `routes.js` paths. `/shop-all` (Shop All landing) ≠ `/shop/all-hair-color` (filtered category view).

**Testing edge case:** Vuex 4 `commit` spy: use `commitSpy.mock.calls.find(call => call[0] === 'global/setBreadcrumbs')` then `toEqual` on `call[1]` — `toHaveBeenCalledWith` fails due to extra `options` arg.

**Validated:** DOTCOMPB-7944 — hardcoded `/shop/all-hair-color` instead of `/shop-all`. Single point of failure found via grep.
