---
title: CMS Partials — Anatomy, Render Flow, and Scaffolding a New Partial from Zero
impact: HIGH
impactDescription: Partials are the mechanism by which CMS-configurable HTML+CSS blocks are loaded at runtime into the site (promo callouts, banners, marketing partials). Without this rule, authors mis-classify a partial as a component, forget the paired content document, fail to globally register the Vue tags used inside the partial's jade, or place the Vue component in the wrong lifecycle hook for SSR.
tags: partial, partial-loader, partial-preview, cms-partial, cms-partial-ssr, CMSPartial, CmsPartialSsr, mixin_key, mixinKey, getPartial, loadPartial, renderPartialContent, generatePartialHtml, partial template, partial content, contentVersion.templateKey, registerGlobalsSsr, mrVueApp, defineAsyncComponent, htmlComponent, SSR, serverPrefetch, experiment, audience, trackExperiment, cmsSettings, settings.partial
---

# CMS Partials

A **partial** is a CMS-configurable HTML+CSS block, identified by a `mixin_key`, that is rendered at runtime by the `<cms-partial>` (or `<cms-partial-ssr>`) Vue component. Use partials when a section of the site needs to be **swappable per campaign / per audience / per A/B variation without a code deploy**: promo callouts, marketing banners, blog inserts, post-cancel modals, return-customer welcome blocks, sticky drawers, etc.

## 1. The Three Moving Parts

Every partial requires **all three** of the following to exist. If any one is missing, the partial fails silently with a `getPartial` 404 (cf. `mr_modules/cms/lib/loaders.js:184`).

| Part | Collection | Identified by | Purpose |
|---|---|---|---|
| **Partial template** | `template` + `templateVersion` | `template.mixin_key` (the *jade owner*) | Defines the jade + stylus that produces the partial's HTML/CSS. May reference Vue tags. |
| **Partial content** | `content` + `contentVersion` | `content.mixin_key` (the *runtime key*) | The configured instance: holds the templateData, may have A/B variations, may belong to an experiment / audience. The `contentVersion.templateKey` points back to the template's `mixin_key`. |
| **Vue mount point** | (code, not CMS) | `<cms-partial mixin-key="…">` in some page template's jade | The component that loads the partial via `cms/loadPartial` and renders the resulting HTML. |

The **`mixin_key` conventions** in this codebase:

* **Content `mixin_key`** is conventionally prefixed `partial-` (e.g., `partial-urm-perks`, `partial-hellospring-mar-25-rcd-banner`, `partial-take-quiz-blog`, `partial-email-capture-block`, `partial-post-sub-cancel-rcc`). This is the key callers pass to `cms-partial(mixin-key="…")`.
* **Template `mixin_key`** has two patterns:
  - **Dedicated** (1:1 with a single content) — template key matches or closely mirrors content key (e.g., `sugg-limitless-pro-template` for content `sugg-limitless-pro-template`).
  - **Reusable** (1:many) — short shape-only template name (e.g., `thick-banner-v4` template renders dozens of seasonal banner contents: `partial-hellospring-mar-25-rcd-banner`, `partial-may-2025-prospect-promo-drawer`, …).

**Pick the reusable pattern when** the visual is recurring (banner, drawer, callout) and you want marketing to spin up new variants without engineering. **Pick the dedicated pattern when** the partial is one-of-a-kind (a specific page section) — it's easier to evolve fields safely when no other content depends on the template.

## 2. The `type` Field — What It Really Controls

`template.type` is one of `partial`, `component`, `container`, `layout`. **The runtime treats all of them the same way when loaded via `cms-partial`** — `getPartial(mixinKey)` → `loadPageAsync(mixinKey)` looks up by content `mixin_key`, then loads the template by `contentVersion.templateKey`, regardless of template `type`.

The `type` controls only:

* **Tophat UI grouping** — how templates surface in the editor's library.
* **Mounting semantics for non-partial render paths** (`type=layout` means the template is mountable as a page-level layout; `type=container` means it can host nested components in `settings`).

Real-world evidence in this codebase:

* `partial-urm-perks` (mixin_key) → mounted by template `urm-perks` (`type=component`, _id 1308 family).
* `partial-hellospring-mar-25-rcd-banner` → template `thick-banner-v4` (`type=component`, _id 1308).
* `sugg-limitless-pro-template` content → template `sugg-limitless-pro-template` (`type=partial`, _id 1634). **Only 1 template in the entire CMS has `type=partial`** as of 2026-05-11.
* `partial-take-quiz-blog`, `partial-email-capture-block` — used by blog layout (template 1170) via `cms-partial-ssr`.

**Rule of thumb**: don't agonize over `type`. Use `type=partial` only when the template is truly partial-only (won't be mounted as a top-level page, won't host nested components). Otherwise `type=component` is fine and matches the dominant precedent.

## 3. Render Flow End-to-End

The full chain when a page renders a `<cms-partial mixin-key="partial-foo">`:

```
PAGE TEMPLATE JADE
  cms-partial(mixin-key="partial-foo")
        │
        ▼
CMSPartial.vue  (or CmsPartialSsr.vue)         website/src/vuescripts/components/CMSPartial/
  • serverPrefetch / created
  • this.loadPartial({ mixinKey: 'partial-foo' })
        │
        ▼
store/modules/cms.js — loadPartial action
  • memoised: if state.cmsPartials['partial-foo'] exists, no-op
  • vueCmsSvc.getPartial({ mixinKey: 'partial-foo' })
        │
        ▼
services/vueCmsSvc.js
  GET /api/cmsSvc/getPartial?mixinKey=partial-foo
        │
        ▼
mr_modules/webservices/lib/cmsSvc.js — getPartial(params, req, callback)
  • cms.generatePartialHtml(mixinKey, req, res)
        │
        ▼
mr_modules/cms/lib/utils.js — generatePartialHtml
  • htmlRenderer.renderPartialContent(mixinKey, req, res)
        │
        ▼
mr_modules/cms/lib/htmlRenderer.js — renderPartialContent
  • loader.loadPageAsync(mixinKey)
      ├─ liveLoadContent(criteria) — query content collection by mixin_key
      ├─ getContentVariation(req, res, defVariant, content) — pick A/B variation
      └─ liveLoadTemplate(content.templateKey) — query template collection
  • renderPartialHtml(content, template, req, locals)
      • renderContainerAsync — compile jade with locals = {settings, content, params, components, …}
      • addCmsInfo — inject cms data attributes for debug
      • caches result
  • renderContentCss(content) — compile stylus
  • returns { html, css, mixinKey, trackExperiment, trackAudienceContent }
        │
        ▼
CMSPartial — commit('setCmsPartial', {key, val})
  • computed.htmlComponent = { template: html, props: {...} }
  • template: component(:is="htmlComponent")  ⟵  Vue runtime-compiles the partial HTML
  • Vue tags inside the partial HTML resolve against globally registered components
  • mounted: handleTrackExperiment fires 'Experiment Viewed' if content.experimentId present
```

**Key file paths to remember**:

| Layer | Path |
|---|---|
| Vue mount point (general) | `website/src/vuescripts/components/CMSPartial/CMSPartial.vue` |
| Vue mount point (SSR-curated) | `website/src/vuescripts/components/CmsPartialSsr/CmsPartialSsr.vue` |
| Vuex action / state | `website/src/vuescripts/store/modules/cms.js` (`loadPartial`, `getPartial`, `cmsPartials`) |
| Frontend service | `website/src/vuescripts/services/vueCmsSvc.js` (`getPartial`, `getPartials`) |
| Webservice (REST) | `mr_modules/webservices/lib/cmsSvc.js` (`getPartial`, `getPartials`) |
| Render engine | `mr_modules/cms/lib/htmlRenderer.js` (`renderPartialContent`, `renderPartialHtml`) |
| Loader | `mr_modules/cms/lib/loaders.js` (`liveLoadContent`, `liveLoadTemplate`, `loadPageAsync`) |
| Public entry | `mr_modules/cms/lib/utils.js` (`generatePartialHtml`, `generatePartialsFromMixinKeys`) |

## 4. `partial-loader` vs `partial-preview` — the Two Meta-Templates

Template 1319 (`partial-loader`) and template 1293 (`partial-preview`) are the two meta-templates that wrap the `cms-partial` Vue tag.

### `partial-loader` (template 1319, `type=component`)

```jade
if settings.partial
  cms-partial(mixin-key=settings.partial)
```

It's a **CMS-configurable wrapper**: the *parent* template declares a field of type `partial` (or just `text`) named `partial`, and the Tophat author picks the partial's `mixin_key` at edit time. Use this when:

* The slot needs to swap between different partials per campaign without code changes.
* The same wrapper layout (padding, container) is reused across many promo variants.
* A `type=partial` field is added to the parent template's config (Tophat renders a partial picker — see `template-field-schema.md` for the `type=partial` field).

### `partial-preview` (template 1293, `type=component`)

A preview-only template used by Tophat's preview UI. **Not a runtime path** — don't reach for this when shipping production code. Its templateVersion is `version: 0` (not even staged), which is a strong tell.

### When to use them vs direct `cms-partial(mixin-key="…")`

| Need | Pattern | Example |
|---|---|---|
| Hardcoded partial reference, fixed layout | Inline in page template's jade | `hcb-founder-membership` template (1444) → `cms-partial(mixin-key="partial-urm-perks")` |
| CMS-configurable partial selection | `partial-loader` wrapper + parent template's `type=partial` field | Promo rotation slot |
| Blog post inline blocks (SSR-curated components allowed inside the partial) | `cms-partial-ssr(mixin-key="…")` inline | Blog layout 1170 → `cms-partial-ssr(mixin-key="partial-take-quiz-blog")` |
| Page-section partial mounted from a Vue component | `<cms-partial :mixin-key="cmsSettings.colorCrewModule.cms_partial">` (Vue binding) | `ColorKitPdpV2.vue:35`, `ColorKitPdpV3.vue:36` |

## 5. Vue Component Resolution Inside a Partial's HTML — A Critical Gotcha

When `<cms-partial>` renders the partial's compiled HTML, it does so via Vue 3's runtime template compiler:

```js
htmlComponent() {
  return {
    template: this.html,            // the compiled partial HTML, possibly containing Vue tags
    props: { clientConfig: {...} },
  };
}
// template usage:
//   component(:is="htmlComponent" v-bind="fullBinding" :client-config="config")
```

Any custom Vue tag inside the partial's jade — e.g., `mr-btn(...)`, `img-box(...)`, `offer-callout(...)` — is resolved at runtime against:

1. **`htmlComponent.components`** (local components on the dynamic component definition).
2. **The current app's globally-registered components** (registered via `app.component(name, def)` in `mrVueApp.js` and `registerGlobalsSsr.js`).

**`CMSPartial.vue` declares NO local components.** Therefore every custom Vue tag in a partial rendered by `<cms-partial>` MUST be globally registered on both the client app *and* the SSR app.

**`CmsPartialSsr.vue` declares a curated local set** (`EmailCaptureBlock`, `StoreValue`, `MrBtn`, `MrIcon`). Use this variant when your partial needs those specific components and you don't want to globalise more.

### Practical implications

* **Picking a Vue component for a partial?** Verify it's globally registered. Check `website/src/vuescripts/mrVueApp.js` (client) and `website/src/vuescripts/ssr/registerGlobalsSsr.js` (SSR). The line `app.component('my-component', MyComponent)` must exist in both — kebab-case tag name.
* **Adding a new globally-registered component for a partial?** Update *both* files. The `defineAsyncComponent(() => import('@components/Foo/Foo'))` pattern is fine and is the norm for code-splitting.
* **Component is page-local (mounted only inside a Vue component tree)?** Don't register globally — use it directly in the parent component instead. Don't route it through a partial.

### Pre-registered global components available inside any `<cms-partial>` partial

A non-exhaustive list (see `registerGlobalsSsr.js` for the source of truth as of 2026-05-11):

`img-box`, `video-box`, `cms-expose-settings`, `party`, `toggle`, `slick`, `responsive-image` (legacy), `add-promo`, `mr-spinner-veil`, `transition-expand`, `store-value`, `mr-navigation`, `mr-sticky-header`, `chat-widget`, `app-modal`, `site-message-banner-carousel`, `site-message-banner`, `toggle-interval`, `interval`, `email-capture-modal`, `phone-capture-modal`, `notifications`, `sticky-promo-drawer`, `mr-bday`, `breadcrumbs`, `simple-carousel-ssr`, `reactive-to-items-carousel`, `accordion`, `accordion-section`, `tabs`, `faqs`, `reviews`, `hair-color-bar-booking-v2`, `color-bar-location-section-v1`, `cms-partial`, `cms-partial-ssr`, … plus all the homepage / blog / shop / dashboard module components.

## 6. Author Workflow — Scaffold a New Partial from Zero

The end-to-end recipe. **Order matters** — if you create the content before the template, `loadPageAsync` resolves the content but `liveLoadTemplate` 404s on its `templateKey`.

### Step 1 — Build the Vue component (if a custom Vue tag is used)

Skip this step if your partial is pure HTML/CSS (e.g., a banner with images and links only).

```
website/src/vuescripts/components/OfferCallout/
├── OfferCallout.vue        # PascalCase file, kebab-case tag = <offer-callout>
├── OfferCallout.test.js    # co-located vitest suite
└── index.js                # barrel: export { default } from './OfferCallout.vue'
```

Component contract:

* **Props match the partial template's `settings` interpolation** — every `${settings.foo}` in the partial jade becomes a `foo` prop (camelCase) on the Vue component.
* **Bound via attributes in the partial jade**, e.g.:
  ```jade
  offer-callout(
    :copy=`'${settings.copy}'`
    :promo-code=`'${settings.promoCode}'`
    :promo-name=`'${settings.promoName}'`
    :cta-text=`'${settings.ctaText}'`
    :cta-destination=`'${settings.ctaDestination}'`)
  ```
  Note: `:foo='${settings.foo}'` is jade-side string interpolation that produces a *Vue binding expression*. The outer `\`…\`` is the jade template literal; the inner `'${…}'` is a JS string literal embedded into the Vue binding so the runtime template compiler sees `:foo="'actual string value'"`. Don't write `foo=settings.foo` for non-string types — use `:foo` (Vue binding) for booleans, numbers, objects, arrays.
* **Lifecycle**: code that touches `window` / `document` / `localStorage` must live in `mounted` (or be guarded by `import.meta.env.SSR` / `typeof window !== 'undefined'`). The partial's compiled template runs on the server during SSR and on the client during hydration. See `.claude/rules/ssr-safety.md`.

### Step 2 — Globally register the component (both files)

`website/src/vuescripts/mrVueApp.js` — client registration. Find an existing registration of a similar component, mirror the pattern (sync `import` for components used on every page, `defineAsyncComponent` for code-splitting).

`website/src/vuescripts/ssr/registerGlobalsSsr.js` — SSR registration. Same module, same name. **Both must use the same kebab-case tag name** (e.g., `app.component('offer-callout', OfferCallout)`).

Verify after registration:

```bash
grep -n "offer-callout\|OfferCallout" website/src/vuescripts/mrVueApp.js website/src/vuescripts/ssr/registerGlobalsSsr.js
```

Two hits in each file (one import, one `app.component(...)` call) is the expected shape.

### Step 3 — Create the partial template via the skill script

The skill ships **`create-partial-template.mjs`** so you never hand-type a mongosh insert. It is idempotent (refuses to re-create if the `mixin_key` already exists), dry-runs by default, and writes a rollback record to `cms-backups/template/<mixin_key>/<stamp>-create.json` on `--confirm`.

Always inspect a similar reference partial first to confirm the shape you want to mirror:

```bash
# Reference: inspect existing partial templates to compare
node ~/.claude/skills/tophat-tools/scripts/inspect-template.mjs sugg-limitless-pro-template  # the 1 type=partial template
node ~/.claude/skills/tophat-tools/scripts/inspect-template.mjs urm-perks                     # type=component with config[] schema
node ~/.claude/skills/tophat-tools/scripts/inspect-template.mjs thick-banner-v4              # reusable shape-only template
```

Author the spec file (`.tasks/DOTCOMPB-8120/offer-callout-template.json` or similar — kept in `.tasks/<ticket>/` per the *Generated Files Policy* in the project CLAUDE.md):

```json
{
  "mixin_key": "partial-marketing-lp-offer-callout",
  "name": "Marketing LP Offer Callout",
  "type": "component",
  "targetPlatform": "desktop",
  "jade": "if settings.copy && settings.promoCode\n  offer-callout(\n    :copy=`'${settings.copy}'`\n    :promo-code=`'${settings.promoCode}'`\n    :promo-name=`'${settings.promoName}'`\n    :cta-text=`'${settings.ctaText}'`\n    :cta-destination=`'${settings.ctaDestination}'`)",
  "config": [
    { "name": "copy",           "type": "textarea", "options": { "required": true, "rows": 4 }, "helpText": "Offer callout body. Newlines preserved." },
    { "name": "promoCode",      "type": "text",     "options": { "required": true },             "helpText": "Promo code applied on /colorbar/locations?promo=<code>." },
    { "name": "promoName",      "type": "text",     "options": { "required": true },             "helpText": "Display name used in Segment tracking." },
    { "name": "ctaText",        "type": "text",     "default":  "Apply Offer" },
    { "name": "ctaDestination", "type": "link",     "default":  "/colorbar/locations" }
  ]
}
```

Then dry-run, then commit:

```bash
# Dry-run (default) — prints the prospective insert + counter-allocation plan, no DB writes
node ~/.claude/skills/tophat-tools/scripts/create-partial-template.mjs --src .tasks/<TICKET>/offer-callout-template.json

# Apply — backs up the allocation record to cms-backups/template/<mixin_key>/<stamp>-create.json
node ~/.claude/skills/tophat-tools/scripts/create-partial-template.mjs --src .tasks/<TICKET>/offer-callout-template.json --confirm
```

Verify the result:

```bash
node ~/.claude/skills/tophat-tools/scripts/inspect-template.mjs partial-marketing-lp-offer-callout
node ~/.claude/skills/tophat-tools/scripts/get-template-fields.mjs partial-marketing-lp-offer-callout
```

If you need to amend the `config[]` field schema after creation, use `set-template-fields.mjs --mode merge --confirm` (the established field-editor script). If you need to amend the jade, hit `templateVersion` directly via `mongoEval` — there's no first-class `set-template-jade.mjs` yet.

### Step 4 — Create the partial content via the skill script

**`create-partial-content.mjs`** is the counterpart. It refuses by default if `templateKey` doesn't resolve to an existing template (so the partial doesn't 404 at runtime). Override that guard with `--no-template-check` only when you intend to create the template later (unusual — reverses the recommended order).

Spec file:

```json
{
  "mixin_key": "partial-marketing-lp-offer-callout",
  "name": "Marketing LP Offer Callout",
  "templateKey": "partial-marketing-lp-offer-callout",
  "variationKey": "default",
  "variationName": "Default",
  "templateData": {
    "copy": "20% off your first appointment with code WELCOME20.",
    "promoCode": "WELCOME20",
    "promoName": "Welcome 20",
    "ctaText": "Apply Offer",
    "ctaDestination": "/colorbar/locations"
  }
}
```

Then:

```bash
# Dry-run — checks templateKey existence first
node ~/.claude/skills/tophat-tools/scripts/create-partial-content.mjs --src .tasks/<TICKET>/offer-callout-content.json

# Apply
node ~/.claude/skills/tophat-tools/scripts/create-partial-content.mjs --src .tasks/<TICKET>/offer-callout-content.json --confirm
```

The script's tail prints a verification command (`inspect-partial.mjs`) and a REST round-trip (`curl … getPartial`) so you can confirm in one step.

**The pair must be consistent**: `content.mixin_key` is the *runtime key* (callers), `contentVersion.templateKey` is the *jade owner* (the template's `mixin_key`). They can differ (reusable template pattern: many content docs share one template) or match (dedicated template pattern: 1:1).

### Step 4b — One-shot read of the full partial

After both inserts land, use **`inspect-partial.mjs`** to verify all four documents (content + contentVersion + template + templateVersion) coexist in one read:

```bash
node ~/.claude/skills/tophat-tools/scripts/inspect-partial.mjs partial-marketing-lp-offer-callout
```

The trailing `diagnostics` block confirms `ready: true` when the partial is fully wired. If any piece is missing, the script exits 3 and prints which document is missing.

### Step 5 — Mount the partial from the page's parent template

Two patterns — pick based on whether the slot needs to be CMS-configurable.

**A) Hardcoded** — fixed `mixin_key`, fastest, simplest:

```jade
//- inside the parent template's jade (e.g., location-specific-colorbar-v2, template _id 1650)
if settings.heroSection.offer && settings.heroSection.offer.enabled
  cms-partial(mixin-key="partial-marketing-lp-offer-callout")
```

**B) CMS-configurable** — author picks the partial in Tophat:

Add a `partial` (or `text`) field on the parent template's config:

```js
{ name: 'offerPartialMixinKey', type: 'text', helpText: 'mixin_key of the offer partial to render; leave blank to hide.' }
```

…then in jade:

```jade
if settings.offerPartialMixinKey
  cms-partial(mixin-key=settings.offerPartialMixinKey)
```

Or use the existing `partial-loader` template (1319) as a sub-component if the parent template's config supports nested components.

### Step 6 — Test the partial locally

* **REST round-trip** — verify the partial loads from the API:
  ```bash
  curl -s 'http://localhost:3000/api/cmsSvc/getPartial?mixinKey=partial-marketing-lp-offer-callout' | jq '.data | {html, css}'
  ```
  If you get `{ data: null }` or a 404, one of: content `mixin_key` mismatch, contentVersion `templateKey` mismatch, template doesn't exist, content has no published variation.
* **Page render** — visit the parent page in the browser. The partial should appear with the templateData values from §4.
* **SSR vs CSR** — view the page source (`curl -s http://localhost:3000<uri> | grep -A 5 'cms-partial'`) to confirm the partial HTML is present in the server-rendered HTML (signals `serverPrefetch` ran). If only `<div class="cms-partial-wrapper"></div>` appears, the partial is loading client-side only — usually fine, but check that `state.global.isVueSSRApp` is true on the page.

### Step 7 — Production replication (Tophat hand-off)

Local mongosh mutations affect only the dev environment. To ship: Carley (or whoever owns Tophat-side production replication) replicates the template + content via Tophat's authoring UI in staging then prod. Document the steps in the PR's **Special Deployment Requirements** section per the `pr-scribe` skill.

Alternative: use `migrate-content-experiment.mjs` (see `content-migration` rule) if the new partial belongs to an existing experiment migration plan.

## 7. SSR Considerations

Both `CMSPartial.vue` and `CmsPartialSsr.vue` implement `serverPrefetch` so the partial's HTML is in the SSR output. **What does NOT run on the server**:

* `mounted` — client only.
* `handleTrackExperiment` — guarded by `import.meta.env.SSR` early-return.
* Anything inside the partial's compiled Vue template that touches `window` / `document` without an SSR guard.

**Common SSR pitfalls when authoring partials**:

* The Vue component used inside a partial must not access `window` in `setup` / `created` / `computed` / `data` initialisation. Move that code to `mounted` or wrap with `if (typeof window !== 'undefined')`.
* The partial's CSS is injected as an inline `<style>` tag inside the rendered HTML. This works on both server and client. Stylus is compiled at render time on the server.
* If the partial uses `defineAsyncComponent` for a child component, the chunk loads client-side after hydration; the partial's initial render shows the async component's placeholder (or nothing) until the chunk resolves. Avoid for above-the-fold partials.

## 8. Experiments & Audience Tracking Inside Partials

A partial's `content` document can carry `experimentId` + `variationId` + `weight` (under `contentVersion`). When loaded, `renderPartialContent` builds a `trackExperiment` payload:

```js
trackExperiment = {
  experimentId,
  experimentName: experimentCtl.getExperimentName(experimentId),
  variationId,
  variationName,
  contentName,
  contentId
};
```

…which the Vue mount point fires as a Segment `Experiment Viewed` event on `mounted` (client only, gated by `hasTrackedExperiment` + `lastTrackedKey` to prevent re-firing on watcher updates). See `CMSPartial.vue:handleTrackExperiment`.

Audience matching (DY-like) populates `trackAudienceContent` analogously when `content.audienceMatched` is true.

**Implication for A/B tests on partials**: you don't need a wrapper experiment splitter — bind the content to an experiment in Tophat, and `cms-partial` fires the experiment-viewed event automatically per variation render.

## 9. Editing a Partial — Don't Break the World

* **Renaming the content's `mixin_key`** breaks every `cms-partial(mixin-key="…")` call site that references the old key. Use `find-template-template-usage.mjs` and `grep -rn "the-key"` across the codebase before renaming.
* **Renaming the template's `mixin_key`** breaks the `contentVersion.templateKey` linkage for every content document that points to it. For the *reusable template* pattern, this can be many dozens of content documents. Use `find-template-usage.mjs <oldKey>` to enumerate consumers.
* **Adding a new field to the partial template's `config[]`** is safe (Tophat shows it as empty for existing contents). **Removing a field** breaks the jade if the jade references `settings.fieldThatNoLongerExists` and `undefined` doesn't gracefully degrade — wrap with `if settings.foo`.
* **Mutation discipline**: every change goes through `set-template-fields.mjs --mode merge --confirm` (with `--confirm` only after a successful dry-run), backed up to `website/cms-backups/`.

## 10. Quick Diagnostic Recipes

| Symptom | Diagnostic |
|---|---|
| "Partial doesn't appear on the page." | First check: `node scripts/inspect-partial.mjs <content_mixin_key>` — the `diagnostics` block flags exactly which of the four documents (content / contentVersion / template / templateVersion) is missing. Then `curl 'http://localhost:3000/api/cmsSvc/getPartial?mixinKey=<key>' \| jq` — 404 means content missing; empty `html` means jade ran but produced no output (check `if settings.foo` guards). |
| "Partial appears with raw `<offer-callout>` text — Vue didn't compile it." | The Vue component is not globally registered. Add it to `mrVueApp.js` AND `registerGlobalsSsr.js`. |
| "Partial renders on the client but not in SSR (page source shows empty wrapper)." | Check `state.global.isVueSSRApp` — `CMSPartial.serverPrefetch` only runs when this is true. May indicate Vite SSR mis-config or that the parent page isn't going through `vueSsr.js`. |
| "Partial renders, but `settings.foo` is `undefined`." | `contentVersion.templateData.foo` is missing, or the partial's `config[]` field name doesn't match the jade interpolation, or you're inspecting the wrong contentVersion (e.g., edit_version=2 but published_version=1). Run `inspect-content` to confirm which version is live. |
| "Experiment-viewed event fires twice." | Check `hasTrackedExperiment` gate in `CMSPartial.handleTrackExperiment` — the watcher on `mixinKey` re-runs `init` and the gate should suppress re-tracking unless the key changed. |
| "Partial is cached and won't update after a templateData change in dev." | The renderer caches HTML by `getCacheKey(content, template, req)`. In dev, this rarely persists across reloads, but if `liveLoadContent` is hitting the live collection (vs dev), changes go to `content` not `production_content` — and the renderer reads `production_content` in non-dev modes. Check `config.cms.env` and which collection the dev loader is querying. |

## 11. Scripts Shipped For Partials (`tophat-tools v1.1.0`)

Three scripts cover the partial lifecycle. All three honour `--container <name>` and `--db <name>` standard flags; mutation scripts honour the `--confirm` discipline (dry-run by default, backup before write, idempotent re-runs).

| Script | Read/Write | Idempotent | Purpose |
|---|---|---|---|
| `scripts/inspect-partial.mjs <content_mixin_key> [--version N] [--json]` | Read | n/a | One-shot dump of the full partial footprint: content + active contentVersion + paired template + active templateVersion + a `diagnostics` block flagging missing pieces. Exits 3 when not fully wired. Replaces 3-4 separate `inspect-content` / `inspect-template` calls. |
| `scripts/create-partial-template.mjs --src <spec.json> [--type partial\|component] [--confirm]` | Write | Yes (skips on duplicate `mixin_key`) | Inserts a new `template` + `templateVersion` for a partial. Backs up the allocation record to `cms-backups/template/<mixin_key>/<stamp>-create.json` on `--confirm`. Validates `config[]` field shape the same way `set-template-fields.mjs` does. |
| `scripts/create-partial-content.mjs --src <spec.json> [--confirm]` | Write | Yes (skips on duplicate `mixin_key`) | Inserts a new `content` + `contentVersion` paired with an existing partial template. Refuses by default if `templateKey` doesn't resolve to an existing template (`--no-template-check` to override). Backs up the allocation record to `cms-backups/content/<mixin_key>/<stamp>-create.json`. |

**Why these instead of inline mongosh inserts:** every other mutation in the skill (`set-template-fields`, `migrate-content-experiment`, `set-experiment-status`, `set-variant-weight`, `add-jsonld-script`, …) follows the same dry-run / backup / `--confirm` discipline so a re-run after a botched first attempt is safe and auditable. Hand-typed mongosh inserts breaks that contract; the scripts also produce a backup record that doubles as Carley's production-replication checklist.

**End-to-end recipe in one paste-able block:**

```bash
# 0. Inspect a reference partial for shape comparison
node ~/.claude/skills/tophat-tools/scripts/inspect-partial.mjs partial-urm-perks

# 1. Author the two spec files under .tasks/<TICKET>/ (gitignored)
#    (see Step 3 + Step 4 above for shapes)

# 2. Dry-run both scripts to verify the prospective inserts
node ~/.claude/skills/tophat-tools/scripts/create-partial-template.mjs --src .tasks/<TICKET>/<name>-template.json
node ~/.claude/skills/tophat-tools/scripts/create-partial-content.mjs   --src .tasks/<TICKET>/<name>-content.json

# 3. Commit — order matters (template first, content second; the content's
#    template-existence check refuses if the template hasn't landed yet)
node ~/.claude/skills/tophat-tools/scripts/create-partial-template.mjs --src .tasks/<TICKET>/<name>-template.json --confirm
node ~/.claude/skills/tophat-tools/scripts/create-partial-content.mjs   --src .tasks/<TICKET>/<name>-content.json   --confirm

# 4. Verify the four documents are coherent
node ~/.claude/skills/tophat-tools/scripts/inspect-partial.mjs <content_mixin_key>

# 5. REST round-trip to confirm the partial renders
curl -s 'http://localhost:3000/api/cmsSvc/getPartial?mixinKey=<content_mixin_key>' | jq '.data | {html, css}'
```

The two spec files plus this 5-step block become the PR's *Special Deployment Requirements* checklist for Carley's production replication — she runs the same scripts against the staging/prod Mongo (or replicates via Tophat's authoring UI, whichever the team prefers).

## 12. Reference Partials in This Codebase

Use these as templates for new partials:

| Pattern | Content `mixin_key` | Template `mixin_key` (templateKey) | Notes |
|---|---|---|---|
| Reusable banner (1:many) | `partial-hellospring-mar-25-rcd-banner` (and 30+ siblings) | `thick-banner-v4` (template _id 1308) | Shape-only template, content holds all data including images. Use for promo banner rotations. |
| Dedicated partial (1:1) | `sugg-limitless-pro-template` | `sugg-limitless-pro-template` (template _id 1634, `type=partial`) | The only `type=partial` template; demonstrates the simplest valid shape. |
| Mounted from a parent template (hardcoded) | `partial-urm-perks` | `urm-perks` | Referenced in `hcb-founder-membership` template (1444) jade. |
| Mounted via `cms-partial-ssr` (curated local components) | `partial-take-quiz-blog`, `partial-email-capture-block` | (various) | Used by blog layout 1170. Partials that need `MrBtn` / `MrIcon` / `StoreValue` / `EmailCaptureBlock` inside their jade. |
| CMS-configurable mixin key | (any) | `partial-loader` (template _id 1319) | Wraps `cms-partial(mixin-key=settings.partial)` — use when the parent template wants to pick the partial at edit time. |
| Mounted from a Vue component (not jade) | `cmsSettings.colorCrewModule.cms_partial` | (CMS-configurable) | `ColorKitPdpV2.vue:35`, `ColorKitPdpV3.vue:36` — the Vue component reads the mixin key from its `cmsSettings` prop and binds it to `<cms-partial :mixin-key="…">`. |
