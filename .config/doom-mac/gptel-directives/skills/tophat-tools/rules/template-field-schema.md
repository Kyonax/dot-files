---
title: Template Field Schema — All 23 Field Types and Their Options
impact: HIGH
impactDescription: Authoritative reference for the `templateVersion.config[]` schema — i.e., what fields a Tophat author sees when editing settings on a template, what types they accept, and what options each type supports. Without this, scripted edits to a template's config produce fields that don't render in Tophat or render wrong.
tags: template, field, schema, config, fieldConfig, type, options, helpText, default, selectOptions, locked, text, html, textarea, boolean, number, link, image, croppedImage, staticImage, staticCroppedImage, component, object, partial, sectionHeader, dateTime, icon, product, productPrice, productType, promotion, featuredReview, specificReview, select, allowMultiples, required, xsClass, mdClass, lgClass, smClass
---

# Template Field Schema

Every CMS template defines its editable fields in `templateVersion.config[]`. Each entry is a field descriptor that tells Tophat what input to render, what to validate, what default to seed, and what helper text to display.

```js
{
  name: "heroTitle",          // property name (becomes `settings.heroTitle` in jade)
  type: "text",               // input type (one of the 23 values below)
  options: { required: true, xsClass: "col-xs-12" },
  locked: false,              // when true, editor must "unlock" before changing
  default: null,              // seed value when a new content variation is created
  selectOptions: [],          // populated for type=select
  helpText: "Main headline text displayed on the tile.",
  fieldConfig: []             // nested fields, only when type=object
}
```

The `set-template-fields.mjs` script normalises any incoming descriptor to this shape (keys it always sets: `name`, `type`, `options`, `locked`, `default`, `selectOptions`; sets `helpText` and `fieldConfig` only if present).

## All 23 field types in production use

Counts surveyed across all `templateVersion` records (live and archived). Legend: ✓ = commonly used, ◌ = rare.

| Type                  | Count  | Renders as                                        | Stores                     | Notes                                                                                                       |
|-----------------------|--------|---------------------------------------------------|----------------------------|-------------------------------------------------------------------------------------------------------------|
| `text`                | 17,296 | Single-line text input                            | string                     | Most common type.                                                                                            |
| `component`           | 3,247  | Sub-template picker                               | reference to another template (settings + componentList) | Use `options.template` (single) or `options.templates[]` (whitelist) to constrain choices. |
| `staticCroppedImage`  | 2,133  | Image picker with editor crop UI                  | media object               | Like `croppedImage` but the rendered image is statically cropped at save (no responsive crop variants).      |
| `croppedImage`        | 1,996  | Image picker with editor crop UI                  | media object + crop rects  | Use `options.aspectRatio`, `imageHeight`, `imageWidth`, `crops`, `customCrops`, `quality`.                  |
| `boolean`             | 1,953  | Checkbox / toggle                                 | boolean                    | `default: true|false`. Common for show/hide flags.                                                           |
| `image`               | 1,788  | Image picker                                      | media object               | Simpler than `croppedImage` — no cropping UI.                                                                |
| `link`                | 1,527  | URL + label compound input                        | `{ url, text }`            | Use `options.allowMultiples: true` for an editable list.                                                     |
| `object`              | 1,352  | Group / nested fieldset                           | nested object              | **Always** has `fieldConfig: [...]` describing the nested fields. Use for "this section has sub-fields".    |
| `staticImage`         | 1,326  | Image picker                                      | media object               | Similar to `image` but emphasises static (non-responsive) usage.                                             |
| `html`                | 589    | WYSIWYG editor                                    | HTML string                | Use `options.rows: N` to size the editor.                                                                    |
| `select`              | 337    | Dropdown                                          | string (choice value)      | Pair with `selectOptions: [{value, label}, ...]`. The editor also accepts a comma-separated `options.selectOptions` string. |
| `number`              | 320    | Number input                                      | number                     | Use `options.min`, `options.max`.                                                                            |
| `textarea`            | 309    | Multi-line text                                   | string                     | Preserves newlines. Use `options.rows: N`.                                                                   |
| `sectionHeader`       | 106    | Visual divider in the editor (no value stored)    | nothing                    | Pure UI hint. Use to group related fields.                                                                   |
| `product`             | 69     | Product picker                                    | product reference          | Drives mr-price / product widgets.                                                                           |
| `dateTime`            | 19     | Date + time picker                                | ISO string                 | `options.required: true` validates non-empty.                                                                |
| `productPrice`        | 18     | Product picker w/ price config                    | `{ productId, subPrice }`  | Used by hero-static and similar.                                                                             |
| `partial`             | 17     | Sub-partial picker                                | partial reference          | Distinct from `component` — picks a CMS partial, not a sub-template.                                          |
| `icon`                | 17     | Icon picker                                       | icon reference             | Drives `<mr-icon>` / `<MrIcon>` rendering.                                                                   |
| `featuredReview`      | 12     | Review picker                                     | review reference           | Domain-specific; ties to the reviews data store.                                                              |
| `productType`         | 5      | Product type / shade-family picker                | product type reference     |                                                                                                              |
| `promotion`           | 5      | Promotion picker                                  | promotion reference        | Ties to `promotionPkg.promoCode`.                                                                            |
| `specificReview`      | 2      | Specific review picker                            | review reference           | Rare; legacy.                                                                                                 |

## `options` keys (per-type cheat sheet)

The most common option keys, grouped by what they do:

| Key                       | Type           | Used by                                                | Purpose                                                                                       |
|---------------------------|----------------|--------------------------------------------------------|-----------------------------------------------------------------------------------------------|
| `required`                | boolean        | Most types                                              | Mark the field as required in the editor.                                                     |
| `xsClass` / `smClass` / `mdClass` / `lgClass` | string | Most types                          | Bootstrap-grid column class (`col-xs-12`, `col-md-6`, etc.) — controls the editor layout.    |
| `allowMultiples`          | boolean        | `link`, `image`, `croppedImage`, `component`, `html`, `partial`, `product`, `featuredReview`, `object` | Render as an editable list rather than a single value.                                       |
| `template` (string)       | string         | `component`, `object`, `croppedImage`, `number`         | Default sub-template mixin_key.                                                               |
| `templates` (array)       | string[]       | `component`, `object`, `croppedImage`, `number`         | Whitelist of allowed sub-template mixin_keys. Empty array = no constraint.                    |
| `useTemplateSelect`       | boolean        | `component`                                             | Force a select-style picker even when `templates` is empty.                                   |
| `aspectRatio`             | number         | `croppedImage`, `image`, `staticCroppedImage`, `staticImage`, `object` | Lock the crop UI to a specific aspect ratio.                                          |
| `imageHeight` / `imageWidth` | number      | `croppedImage`                                          | Pin the rendered image dimensions in pixels.                                                   |
| `crops` / `customCrops`   | array          | `croppedImage`, `staticCroppedImage`, `object`          | Per-breakpoint crop definitions.                                                              |
| `constrainImageSize`      | boolean        | `croppedImage`, `staticCroppedImage`, `object`          | Disallow upscaling beyond the source image.                                                    |
| `quality`                 | number         | `croppedImage`                                          | JPEG/WebP quality 0-100.                                                                       |
| `rows`                    | number         | `html`, `textarea`, `link`                              | Visible rows in the editor.                                                                    |
| `min` / `max`             | number         | `number`                                                | Numeric bounds.                                                                                |
| `width`                   | number         | `image`, `croppedImage`, `component`, `object`          | Display-time width hint.                                                                       |
| `selectOptions` (string)  | string         | `select` (alternative shape)                            | Multi-line `value: Label` pairs (one per line). `selectOptions[]` array is preferred; the string form is legacy. |
| `maxLength`               | number         | `object` (string-mimicking)                             | Validation hint.                                                                               |
| `isModal`                 | boolean        | `component`                                             | The picked sub-template is presented as a modal, not inline.                                   |

## `selectOptions` shape (for `type: "select"`)

Modern shape — array of objects:

```js
{
  name: "contentTheme",
  type: "select",
  options: { xsClass: "col-xs-12" },
  default: "light",
  selectOptions: [
    { value: "light", label: "Light Theme (White Text)" },
    { value: "dark",  label: "Dark Theme (Dark Purple Text)" }
  ],
  helpText: "Select the look of the title and description."
}
```

Legacy shape — comma-separated string in `options.selectOptions` (still parsed by the editor):

```js
options: { selectOptions: "light: Light Theme (White Text),\ndark: Dark Theme (Dark Purple Text)" }
```

`set-template-fields.mjs` accepts either; the modern array shape is preferred when authoring.

## `fieldConfig` (for `type: "object"`)

Object fields nest. `fieldConfig: []` is the same shape as the top-level `config`. Example (truncated from template 1625):

```js
{
  name: "ShowcaseCarousel",
  type: "object",
  options: { xsClass: "col-xs-12" },
  helpText: "A featured content module ...",
  fieldConfig: [
    { name: "primaryTile", type: "object", helpText: "Main featured content area",
      fieldConfig: [
        { name: "contentTheme", type: "select", default: "light", selectOptions: [...] },
        { name: "title",        type: "html",   options: { required: true } },
        { name: "ctaUrl",       type: "link" },
        { name: "metadata",     type: "object", helpText: "Advanced configuration",
          fieldConfig: [
            { name: "autoplay", type: "boolean" },
            { name: "mute",     type: "boolean" }
          ]
        }
      ]
    }
  ]
}
```

`get-template-fields.mjs --flat` flattens the nested structure into dot-path notation (`ShowcaseCarousel.primaryTile.metadata.autoplay`) — useful when you need a complete inventory of every leaf field.

## Where the values are stored at render time

Field values flow:

```
templateVersion.config[]   →   contentVersion.templateData[fieldName]
   (schema)                          (instance)
```

For top-level page templates, the page's componentList is on `contentVersion.templateData.componentList`. Each entry references a sub-template (mixin_key) and provides settings shaped by that sub-template's `config`.

For sub-templates, the editor stores the field values in the parent's componentList entry as `settings` — e.g., a `location-specific-colorbar-v2` entry stores `{ title: "...", subtitle: "...", colorbarTitle: "..." }` because those are the three fields its config defines.

## Authoring workflow

```sh
# 1. Get the current schema as JSON.
node get-template-fields.mjs <template> --json > /tmp/<template>-fields.json

# 2. Edit by hand or via another script. Add helpText, change types, append a field, etc.

# 3. Dry-run the change.
node set-template-fields.mjs <template> --src /tmp/<template>-fields.json
# Review the added/removed/kept summary.

# 4. Apply.
node set-template-fields.mjs <template> --src /tmp/<template>-fields.json --confirm
```

Common edits the script supports out of the box:

* Add a new field (just append to the JSON array).
* Add `helpText` to fields that lack it (Tophat 1625 is the gold standard for help-text density).
* Convert a `text` field to `select` by setting `type: "select"` and adding `selectOptions: [...]`.
* Wrap related fields in an `object` group (move them into a parent's `fieldConfig`).
* Set `required: true` via `options.required`.
* Lock a field with `locked: true` (editor must explicitly unlock).

## Cross-references

* Reference templates with rich schemas: `_id=17` (`hero-static`, simple flat schema), `_id=1625` (`home-tabs-vs`, deeply nested with helpText on every field).
* `find-template-template-usage.mjs` — finds every other template that references a given template via a `component`-typed field option or a jade embed.
* `get-template-fields.mjs --json` is the canonical starting point for `set-template-fields.mjs --src`.
* See `rules/cms-data-model.md` for how templateVersion.config relates to contentVersion.templateData.
