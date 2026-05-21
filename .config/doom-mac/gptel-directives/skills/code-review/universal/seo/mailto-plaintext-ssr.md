---
id: rule-u-seo-008
title: No Plaintext mailto in SSR HTML
severity: MEDIUM
tags: mailto:, href="mailto, email, useObfuscatedEmail
---

`href="mailto:user@domain"` rendered directly into SSR / SSG HTML is harvested by spam scrapers. Email CTAs must either (a) assemble the `mailto:` href client-side in `onMounted`, or (b) split user/domain into data attributes and build the href on user interaction. The rendered SSR HTML must contain neither `mailto:` nor `user@domain` plaintext for the contact address.

### Apply
- Any `<a href="mailto:…">` in `.vue` / `.jsx` / `.html` templates
- Static HTML files in `public/`
- Email rendering through link/button components (`<UiLink href="mailto:…">`, `<Button as="a" href="mailto:…">`)
- JSON-LD `Person.email` / `Organization.email` is **out of scope** for this rule — that's a separate trade-off (entity confidence vs harvesting). Skip those fields here.

### Skip
- JSON-LD email fields (handled separately)
- `noindex` admin or internal-only routes
- Build-time `dist/` checks (this rule scans source, not output — verify SSR output as a follow-up grep)

### Bad
```html
<UiLink href="mailto:kyonax.corp@gmail.com" external>
  {{ t('contact.cta') }}
</UiLink>
```

### Good
```js
// src/composables/use-obfuscated-email.js
import { onMounted, ref } from 'vue';
const _SSR_PLACEHOLDER = '#';

export const useObfuscatedEmail = (user, domain) => {
  const href = ref(_SSR_PLACEHOLDER);
  onMounted(() => {
    href.value = `mailto:${user}@${domain}`;
  });
  return href;
};
```

```html
<UiLink :href="contact_email_href" external>
  {{ t('contact.cta') }}
</UiLink>
```

### Edge
SSR + the initial CSR ref value must match (`'#'` in both) or Vue logs a hydration mismatch. Updating `href.value` inside `onMounted` patches the DOM after hydration completes — no mismatch warning, no email leakage. For users with JS disabled, the link falls back to `#` (scroll-to-top); accept this trade-off or expose a contact form as the no-JS fallback. Right-click "copy link" before hydration captures `#`, which is harmless but slightly surprising.
