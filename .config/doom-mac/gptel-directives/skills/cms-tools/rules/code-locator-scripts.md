---
title: CMS → Code Locator Scripts
impact: MEDIUM
impactDescription: Bridges between CMS metadata (mixin_key, URI) and the actual codebase (Vue component file, Express route handler). Without these, you'd grep the whole repo every time you wanted to know "what code does this CMS block run?" — a token tax on every CMS investigation.
tags: code-locator, find, mixin_key, vue-component, mrVueApp, registerGlobalsSsr, express-route, views.js, endpoints.js, before.js, contexts.js, kebab-pascal, defineAsyncComponent, cms-partial
---

# CMS → Code Locator Scripts

These are filesystem-only scripts (no Mongo). They translate CMS-side identifiers into the file paths and lines where the corresponding code lives.

## The kebab ↔ PascalCase contract

Every globally-registered Vue component obeys this rule:

| Form                    | Where seen                                          |
|-------------------------|-----------------------------------------------------|
| `kebab-case`            | CMS `mixin_key`; CMS template `jade` (`<location-specific-colorbar-v2>`); rendered HTML tag |
| `PascalCase`            | Component file (`LocationSpecificColorbarV2.vue`); folder name; `app.component('PascalCase', ...)` registration |

The mapping is mechanical: split on `-`, capitalise, join. `find-cms-component-code.mjs` does the conversion both ways and greps the registration files.

## `find-cms-component-code.mjs <mixin_key|tag>`

```sh
node find-cms-component-code.mjs location-specific-colorbar-v2
```

Output:

```json
{
  "mixin_key": "location-specific-colorbar-v2",
  "pascalName": "LocationSpecificColorbarV2",
  "registrations": [
    { "file": "website/src/vuescripts/mrVueApp.js", "line": "968:..." },
    { "file": "website/src/vuescripts/ssr/registerGlobalsSsr.js", "line": "124:..." },
    { "file": "website/src/vuescripts/ssr/registerGlobalsSsr.js", "line": "282:..." }
  ],
  "componentFiles": [
    "website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2/LocationSpecificColorbarV2.vue"
  ],
  "componentFolders": [
    "website/src/vuescripts/components/LocationSpecific/LocationSpecificColorbarV2"
  ],
  "registrationHint": null
}
```

Three possible outcomes:

1. **Both `registrations` and `componentFiles` populated** → the mixin_key is a globally registered Vue component. Read the file at `componentFiles[0]`.
2. **Only `registrations` populated, no file found** → registration points at an alias path that didn't resolve. Run `grep -n "PascalName" website/src/vuescripts/mrVueApp.js` and follow the import path manually.
3. **Neither populated; `registrationHint` set** → the mixin_key is most likely a *CMS partial* (HTML body authored entirely in Tophat's `template.jade`, not backed by Vue code). Use `inspect-template.mjs <mixin_key>` to read the pug body.

## `find-route.mjs <uri>`

```sh
node find-route.mjs /colorbar/locations
node find-route.mjs /api/cmsSvc/getPartial
```

Greps the Express routing layer:

| File                                       | Purpose                                                |
|--------------------------------------------|--------------------------------------------------------|
| `website/src/routing/routing.js`           | Top-level mounts.                                       |
| `website/src/routing/before.js`            | Pre-route middleware (cookies, auth, experiment alloc). |
| `website/src/routing/contexts.js`          | Builds `req.context` from CMS data.                     |
| `website/src/routing/views.js`             | Page handlers — most of these are CMS catch-alls.       |
| `website/src/routing/endpoints.js`         | API mounts (`/api/...`).                                |
| `website/src/routing/endpoints/`           | Per-endpoint files.                                     |

If the only matches are in `views.js`, the script appends a `cmsCatchAllNote` reminding you that the URL is served via the CMS catch-all (`mr_modules/cms/lib/loaders.js#getContent`) and pointing to `inspect-content-by-uri.mjs` for the matching content document.

## Common workflows

### "I see this kebab-case tag in rendered HTML — where is its code?"

```sh
node find-cms-component-code.mjs location-specific-colorbar-v2
```

### "What happens when this URL is requested?"

```sh
# 1. Express handler (if any).
node find-route.mjs /colorbar/locations

# 2. CMS document (if catch-all served).
node inspect-content-by-uri.mjs /colorbar/locations

# 3. Block-by-block, what Vue components render.
node get-component-list.mjs <contentId> --variation A
# For each mixin_key in the output:
node find-cms-component-code.mjs <mixin_key>
```

### "I'm renaming a mixin_key — what code needs to change?"

```sh
# 1. CMS-side ripple.
node find-template-usage.mjs <old-mixin-key>

# 2. Code-side ripple — the global registration files + the component folder.
node find-cms-component-code.mjs <old-mixin-key>

# Update both:
#   - Tophat sub-template's `mixin_key` field (or write a fresh template + retire the old).
#   - mrVueApp.js + registerGlobalsSsr.js: rename the kebab tag and the PascalCase identifier.
#   - components/<old-folder>/ → components/<new-folder>/, rename .vue and folder.
```

## Limitations / gotchas

* **Nested global registrations** — the script greps line-level matches. A registration that spans multiple lines (very rare; not used in this codebase) won't be detected.
* **Auto-imports / virtual modules** — Vite's import.meta.glob is used in some places (notably the modal system in `AppModal.vue`). A CMS partial that resolves via glob won't show as a hand-written registration. Search the codebase manually if `find-cms-component-code` returns the partial hint.
* **Alias resolution** — the script takes `@components` references at face value (mapped to `website/src/vuescripts/components`). If you've added a new alias, update `vite/vite.common.config.mjs` accordingly.

## Cross-references

* Global Vue registration: `website/src/vuescripts/mrVueApp.js`, `website/src/vuescripts/ssr/registerGlobalsSsr.js`
* Path aliases: `website/vite/vite.common.config.mjs`
* CMS catch-all: `mr_modules/cms/lib/loaders.js#getContent`
* CMS partial runtime: `website/src/vuescripts/components/CMSPartial/CMSPartial.vue`
