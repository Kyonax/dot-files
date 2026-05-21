---
id: rule-u-seo-009
title: HSTS Header Declared in Server Config
severity: MEDIUM
tags: Strict-Transport-Security, hsts, .htaccess, _headers, Header always set
---

The production server must emit `Strict-Transport-Security` so browsers refuse plaintext HTTP requests. Ratchet conservatively: start with `max-age=15552000` (180 days) without `preload`; promote to `max-age=31536000; includeSubDomains; preload` only after 2+ weeks of clean HTTPS across all subdomains.

### Apply
- `public/.htaccess`, `_headers`, `vercel.json`, `netlify.toml`, `nginx.conf`, `caddy.json`, `Caddyfile`, `cloudfront-functions/**`, any file declaring response headers
- Files matching `**/headers*.{js,ts,json}` exporting middleware response headers (Express `helmet`, Hono, SvelteKit `hooks.server.ts`)

### Skip
- Local-only / staging configs not deployed publicly
- Sites still rolling out HTTPS — flag as a TODO, not a violation

### Bad
```apache
# HSTS — enable AFTER 1-2 weeks of clean HTTPS. Sticky in browsers.
# Header always set Strict-Transport-Security "max-age=31536000; includeSubDomains; preload"
```

### Good
```apache
# Stage 1, conservative. max-age=180d, no includeSubDomains, no preload.
# Ratchet to "max-age=31536000; includeSubDomains; preload" after 2 weeks of clean HTTPS.
Header always set Strict-Transport-Security "max-age=15552000"
```

### Edge
HSTS is sticky in browsers. Jumping straight to `preload` + `includeSubDomains` + `max-age=31536000` locks every browser visitor to HTTPS for a year — if HTTPS breaks (cert expiry, subdomain misconfigured), users cannot recover without manually clearing HSTS state. Always stage. The `preload` directive additionally submits the domain to the Chrome HSTS preload list, which requires manual delisting (weeks). Helmet's default `hsts` middleware already ships `max-age=15552000` + `includeSubDomains` — auditing Helmet config alone may satisfy this rule on Node servers.
