---
id: rule-pj-mrb-004
title: NEVER Rewrite req.url — CMS and Vue Router SSR Are Coupled
severity: CRITICAL
tags: req-url, ssr, hydration, mismatch, rewrite
---

`req.url` couples the CMS `htmlRenderer` and Vue Router SSR — NEVER rewrite it in middleware or route handlers because it causes hydration mismatch between server and client.

### Apply
- Any Express middleware that modifies `req.url`, `req.originalUrl`, or `req.path` before the CMS SSR pipeline
- Custom redirect logic that mutates the request URL instead of sending a proper HTTP redirect
- Debugging hydration mismatch errors where server-rendered HTML does not match client-side Vue Router state

### Skip
- Proxy middleware (e.g., Vite dev server proxy) that operates on a separate middleware stack before CMS processing
- `res.redirect()` calls that send an HTTP 301/302 — these do not mutate `req.url`

### Bad
```javascript
// Express middleware that rewrites req.url before CMS processing
app.use((req, res, next) => {
  if (req.url.endsWith('/reviews')) {
    req.url = req.url.replace('/reviews', '');  // NEVER DO THIS
  }
  next();
});
```

### Good
```javascript
// Use a proper HTTP redirect if the URL must change
app.use((req, res, next) => {
  if (req.url.endsWith('/old-reviews')) {
    return res.redirect(301, req.url.replace('/old-reviews', '/reviews'));
  }
  next();
});

// Or handle the full path natively in both CMS parseUrl and Vue Router config
```

### Edge
The CMS `htmlRenderer` reads `req.url` to resolve the CMS page and inject SSR context. Vue Router's `createMemoryHistory` also receives `req.url` to determine the initial route. If these two see different URLs, the server renders one page while the client hydrates a different route — producing a full-page hydration mismatch that causes the client to discard the SSR HTML and re-render from scratch, destroying performance and causing visual flicker.
