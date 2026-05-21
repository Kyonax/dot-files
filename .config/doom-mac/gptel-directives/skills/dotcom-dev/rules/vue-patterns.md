---
title: Vue 3 Component, Vuex Store, and Modal System Patterns
impact: CRITICAL
impactDescription: Defines how every Vue component, Vuex module, and modal is structured — the foundation of all frontend code in the website.
tags: vue, vue3, vuex, component, store, modal, props, emits, options-api, namespaced, mapState, mapActions, mapGetters, services, mrApi, composables, mixins, global-components, directives, accessibility, aria, semantic, trackMREvent, dayjs, naming, constants, deep, skeleton
---

## Vue 3 Component, Vuex Store, and Modal System Patterns

All frontend UI in the MR website is built with Vue 3 using the **Options API**, **Pug templates**, and **Stylus styles**. This rule covers component structure, Vuex store patterns, the modal system, API services, and global registrations.

---

### 1. Component Directory Structure

Components live in `website/src/vuescripts/components/`. Each component is either a directory (with sub-components and tests) or a single `.vue` file.

**Directory component (preferred for anything non-trivial):**
```
ComponentName/
├── ComponentName.vue         # Main component
├── index.js                  # Re-export: import X from './X.vue'; export default X;
├── ComponentName.test.js     # Co-located tests
└── SubComponent.vue          # Child components
```

**Single file component (simple, standalone):**
```
components/
└── SimpleWidget.vue
```

**Feature-based grouping:**
```
CartAndCheckout/
├── Cart.vue
├── Checkout.vue
├── CartItem.vue
├── routes.js                 # Vue Router route definitions
└── ...
```

---

### 2. Component Script Structure

All components use **Options API**. The canonical field order is:

```javascript
export default {
  name: "ComponentName",        // PascalCase, required

  components: {
    ChildComponent,
  },

  emits: ['update:modelValue', 'confirm', 'cancel'],  // Always declare explicitly

  props: {
    modelValue: [Number, String],
    disabled: { type: Boolean, default: false },
    label: { type: String, required: false },
  },

  data() {
    return {
      localState: null,
    };
  },

  computed: {
    ...mapState('cart', ['loading', 'cartLoaded']),
    ...mapGetters('cart', ['subscriptions']),
    isActive() {
      return !this.disabled && this.localState !== null;
    },
  },

  watch: {
    modelValue(newVal) {
      this.localState = newVal;
    },
  },

  mounted() {
    this.loadInitialData();
  },

  methods: {
    ...mapActions('cart', ['loadCart', 'addToCart']),
    ...mapActions('modal', ['showModal', 'hideModal']),
    loadInitialData() {
      // ...
    },
  },
};
```

**Naming conventions:**
- Component files: **PascalCase** (`MrBtn.vue`, `AddressEditModal.vue`)
- Template usage in Pug: **kebab-case** (`mr-btn`, `address-edit-modal`)
- Props: **camelCase** (`modelValue`, `confirmText`)
- Events/emits: **kebab-case** (`@confirm`, `@update:modelValue`, `@exit-modal`)
- Methods: **action-oriented camelCase** (`handleNavigation`, `checkIsMobile`, `loadData`)
- Data/Computed: **state-reflective camelCase** (`isMobile`, `primaryHeroImage`, `isToday`)
- CSS classes (scoped): **structural kebab-case** (`card-item-details`, `location-gallery`)

**Module-level constants:** Static, non-reactive data (mapping objects, config arrays) must be defined as `const` variables **outside** the `export default` block. This prevents re-declaration on every render:

```javascript
// CORRECT — outside export default
const SWIPER_OPTIONS = { grabCursor: true, slidesPerView: 'auto', spaceBetween: 4 };
const DAY_MAP = { 0: 'Sunday', 1: 'Monday', /* ... */ };

export default {
  name: 'MyComponent',
  // ...
};
```

---

### 3. Styling Architecture (Hybrid Approach)

**Templates use Pug:**
```pug
template(lang="pug")
.my-component.flex.flex-col.gap-md
  h2.f-secondary.upper.xs-f-large.color-mr-purple {{ title }}
  slot(name="content")
  mr-btn(@click="handleClick" :disabled="loading") Save
```

**Utility classes are the primary styling method.** Aggressively apply responsive utility classes in Pug templates. Consolidate common classes on parent elements (DRY principle).

**Scoped Stylus is secondary.** Use `<style scoped lang="stylus">` **only** for:
- Structural layouts and complex positioning
- Pseudo-elements (`:before`, `:after`)
- Transitions and animations
- Complex media queries
- Anything that can't be expressed with utility classes

```stylus
<style scoped lang="stylus">
.my-component
  background-color white
  border 1px solid ui-color-5     // Design system variable
  border-radius 0.5em

  @media mq-mobile                // Design system breakpoint mixin
    padding 1em
</style>
```

**Style rules:**
- Always use `scoped` styles
- Reference design system variables (`ui-color-5`, `mr-purple`) — never hardcode hex
- Use breakpoint mixins (`mq-mobile`, `mq-tablet-plus`, `mq-desktop-md`) — prefer over hardcoded media queries. If no exact breakpoint variable exists for a specific value, keep the hardcoded `px` value rather than using an approximate variable
- `variables.styl` and `mixins.styl` are auto-injected by Vite — no manual import needed
- **Unit convention:** Use `rem` or `em` for layout spacing (margins, padding, width) and typography. For small, fixed-size UI elements like `border-width`, `box-shadow` offsets, or icon sizes, use `px` if the `em`/`rem` conversion results in a non-intuitive decimal (more than two decimal places). Example: `border: 2px solid` is preferred over `border: 0.143em solid`
- **Class ordering in Pug:** Utility classes should be ordered **alphabetically**, except structural/positioning classes which may come first for clarity
- **Alphabetize properties** within `<style>` blocks for consistency
- **Alphabetize computed properties** within the `<script>` block
- **Glassmorphism pattern:** For modern overlays, use `background-color: rgba(...)` + `backdrop-filter: blur(...)`

---

### 4. Vuex Store Module Pattern

All store modules live in `website/src/vuescripts/store/modules/`. Every module is **namespaced**.

```javascript
// store/modules/cart.js

const state = () => ({           // Always a factory function
  cart: {},
  loading: false,
  cartLoaded: false,
});

export const getters = {
  subscriptions(state) {
    return state.cart.items?.filter(item => item.type === 'subscription') || [];
  },
  // Getter with parameters:
  qtyByProductType: (state) => (productType) => {
    return state.cart.items?.filter(i => i.productType === productType).length || 0;
  },
};

export const actions = {
  loadCart({ state, commit, dispatch, getters }) {
    return new Promise((resolve) => {
      vueCartSvc.getCartDataForView({}).then(res => {
        commit('setCart', res.data);
        resolve(res.data);
      });
    });
  },
  // Cross-module dispatch:
  async someAction({ dispatch }) {
    await dispatch('otherModule/otherAction', payload, { root: true });
  },
};

export const mutations = {
  setCart(state, val) {
    state.cart = val;
  },
  setLoading(state, val) {
    state.loading = val;
  },
};

export default {
  namespaced: true,
  state,
  getters,
  actions,
  mutations,
};
```

**Component usage of stores:**
```javascript
import { mapState, mapActions, mapGetters, mapMutations } from 'vuex';

computed: {
  ...mapState('cart', ['loading', 'cartLoaded']),
  ...mapGetters('cart', ['subscriptions']),
},
methods: {
  ...mapActions('cart', ['loadCart', 'addToCart']),
  ...mapMutations('cart', ['setLoading']),
},
```

**Cross-module access in stores:**
- `rootState.moduleName.property`
- `rootGetters['moduleName/getterName']`
- `dispatch('moduleName/actionName', payload, { root: true })`

---

### 5. Modal System (Mandatory Vuex Dispatch)

Modals are **never** mounted with `v-if` or `v-show` directly. All modals go through the centralized `AppModal.vue` component via Vuex.

**Modal component naming:** All modal component file names **must end with "Modal"** — this is validated and enforced.

**Dispatching a modal:**
```javascript
this.$store.dispatch('modal/showModal', {
  component: 'MyFeatureModal',         // Must end with "Modal"
  theme: 'signin',                     // CSS class: signin, bottom-modal, full-screen, etc.
  props: { title: 'Confirm', data: someData },
  persistent: false,                   // Prevent close on background click
  disableClose: false,                 // Show/hide close button
  returnFocusElement: this.$refs.btn,  // Focus management after close
  onClose: () => { /* callback */ },   // Close callback
  desktopEffect: 'modal-body-fade',    // Transition name
  mobileEffect: 'modal-body-fade',     // Transition name
});
```

**Closing a modal:**
```javascript
this.$store.dispatch('modal/hideModal');
```

**Modal state shape** (`store/modules/modal.js`):
```javascript
const state = () => ({
  visible: false,
  modalComponent: null,
  modalProps: {},
  theme: 'default',
  persistent: false,
  disableClose: false,
  stepBackActions: [],
  returnFocusElement: null,
  onClose: null,
  desktopEffect: 'modal-body-fade',
  mobileEffect: 'modal-body-fade',
});
```

**AppModal.vue** renders modals dynamically via `:is="component"`. It handles:
- `<transition>` animations
- `<focus-lock>` for accessibility
- `body-scroll-lock-upgrade` for scroll management
- Step-back navigation for multi-step modals
- In-modal notifications

---

### 6. API Services

Services live in `website/src/vuescripts/services/`. Each domain has a service file.

**Naming convention:** `vue{Domain}Svc.js`
- `vueCartSvc.js`, `vueCustomerSvc.js`, `vueAddressSvc.js`, `vueProductSvc.js`, etc.

**Service file structure:**
```javascript
import mrApi from './mrApi';

export default {
  getCartDataForView,
  addToCart,
  updateItemQty,
};

export function getCartDataForView(params) {
  return mrApi.get('/api/cart/getCartDataForView', { params });
}

export function addToCart(params) {
  return mrApi.post('/api/cart/addToCart', params);
}
```

**mrApi** (available as `this.$mrApi` in components via plugin):
- Wraps Axios with CSRF token injection and timezone headers
- SSR-aware request proxying

---

### 7. Global Components, Directives, and Plugins

**Globally registered components** (~70+ in `mrVueApp.js`):
- `MrBtn`, `MrIcon`, `MrDropdown`, `MrSpinnerVeil`, `ImgBox`
- `AppModal`, `Notifications`, `SiteNav`, `MrFooter`
- `TransitionExpand`, `FocusLock`, `SwiperCarousel`
- And many more — check `mrVueApp.js` for the full list

**Global directives:**
| Directive             | Purpose                                            |
|-----------------------|----------------------------------------------------|
| `v-ripple`            | Material ripple effect on click                    |
| `v-click-outside`     | Detect clicks outside element                      |
| `v-mrscrollanimation` | Scroll-triggered animations (IntersectionObserver) |
| `v-mask`              | Input masking                                      |
| `v-truncate-text`     | Text truncation                                    |

**Global mixins** (7 mixins applied to all components — **legacy, do not create new mixins**):
- `globalMixins` — provides `trackMREvent()`, `scrollTo()`, `goToPath()`, `openLogInModal()`, `addToCartAndTrackEvent()`
- `cartMixin`, `customerMixin`, `advisorMixin`, `xsellMixin`, `trackEventMixin`, `browserContextMixin`

**For new shared logic: use composables, not mixins.**

**Plugins registered:**
- VueAxios + axios, VueCookies, Vue Router 4, PortalVue, Vue Tippy, Google Maps Vue 3, Sentry, custom filters plugin, mrApi plugin

---

### 8. Path Aliases (Vite)

Always use path aliases for imports — never relative paths across directories.

| Alias         | Path                        |
|---------------|-----------------------------|
| `@components` | `src/vuescripts/components` |
| `@services`   | `src/vuescripts/services`   |
| `@store`      | `src/vuescripts/store`      |
| `@utilities`  | `src/vuescripts/utilities`  |
| `@directives` | `src/vuescripts/directives` |
| `@mrModules`  | `../../mr_modules`          |

---

### 9. Component Design Patterns

**Prefer props over `:deep()`:** The `:deep()` selector should be used sparingly. Before using it, always consider if the child's appearance can be controlled via its props API (e.g., adding a `secondary` boolean prop to `MrBtn`). Use `:deep()` only for simple, targeted overrides where modifying a shared child component is not feasible or is overly complex.

**Pre-compute reactive data:** Values calculated once per lifecycle (like `todayDayName`) should be computed properties, not recalculated in methods during render.

**Data preparation in parent:** The parent component prepares data before passing it as props. Use computed properties to shape, filter, or sanitize data for children. Critical example: CMS data (e.g., `heroSettings.defaultLocationImages`) may contain entries without valid `.image.url` properties — the parent MUST filter these out before passing to children.

**No inline event logic:** Template event handlers (`@click`, `@keydown`, etc.) MUST call a single component method. Complex inline expressions or function calls with computed arguments are forbidden as they are not unit-testable:
```javascript
// WRONG — inline logic
@click="trackMREventAndRedirect('Event Name', `/path/${location.code}/page`)"

// CORRECT — dedicated method
@click="handleBookServiceClick"
```

**No magic numbers:** All numeric literals used for configuration, iteration counts, or conditional logic must be extracted into named `const` variables outside `export default`:
```javascript
const VISIBLE_HERO_IMAGES_COUNT = 2;
const THUMBNAIL_SKELETON_COUNT = 3;
const MINIMUM_BREADCRUMB_COUNT = 3;
```

**DRY templates:** For complex or repeated boolean expressions within a `v-for` loop, create a component method that encapsulates the logic:
```javascript
// In methods:
isSelected(slideImage) {
  if (!this.selectedImage || !slideImage) {
    return false;
  }
  return slideImage.url === this.selectedImage.url;
},
```

**Complex feature abstraction:** When a feature needs its own internal state and logic (e.g., image carousel), extract it into a self-contained child component.

**Centralized breakpoint (`global/isDesktop`):** For show/hide logic at the standard desktop threshold (960px+), use `mapGetters('global', ['isDesktop'])` from the Vuex `global` module. This is provided by `mainAppMixin` with a throttled resize listener. **Do not duplicate with local `matchMedia` when the threshold matches.** Only use local `matchMedia` when you need a breakpoint that differs from the global getter (e.g., `max-width: 559px` for mobile-only image layout switching).

**Responsive component swapping:** For radically different mobile vs desktop layouts, use a reactive `isMobile` property to conditionally render entirely different child components. Use `window.matchMedia` (not `window.resize` listeners) for viewport detection — it is more performant and fires only on threshold crossings. Store the media query instance in component `data` (not module-level) to avoid SSR issues:
```javascript
data: () => ({
  isMobile: false,
  mobileMediaQuery: null,
}),
mounted() {
  this.mobileMediaQuery = window.matchMedia('(max-width: 559px)');
  this.handleMediaQueryChange(this.mobileMediaQuery);
  this.mobileMediaQuery.addEventListener('change', this.handleMediaQueryChange);
},
beforeUnmount() {
  this.mobileMediaQuery.removeEventListener('change', this.handleMediaQueryChange);
},
methods: {
  handleMediaQueryChange(event) {
    this.isMobile = event.matches;
  },
},
```

**Shared component modification:** When a shared component needs contextual behavior, add a non-breaking boolean prop (e.g., `staticMode`) — never fork the component.

**Native elements for focus:** Use native `<button>` for clickable actions. `MrBtn` defaults to `<button>` — do not use `tag="a"` unless the element is a true navigation link with a valid `href`. An `<a>` without `href` is NOT keyboard-focusable.

**No nested interactive elements:** Never nest interactive elements (e.g., `<a>` inside `role="button"`). This creates dual tab stops and conflicting handlers. For overlay "view more" patterns inside clickable slides, use a decorative `<div aria-hidden="true">` and handle the action via the parent's single click handler with conditional logic.

**Image fallbacks / skeleton loading:** Use a component-scoped `.has-skeleton` class on containers when images are loading or null, and conditionally render `ImgBox` inside with `v-if`. Both primary and secondary image containers must have `v-if` guards for consistency. Note: `.has-skeleton` is NOT a global utility class — define it in each component's `<style>` block. Complex image components should handle their own skeleton state internally.

**Placeholder functionality:** For UI elements that are placeholders for future features, document with `// TODO:` comments referencing a ticket number if available.

---

### 10. Accessibility, Semantics, and Event Tracking

**Semantic HTML:** Use `<main>`, `<section>`, `<dl>`, `<nav>` over generic `<div>`s wherever appropriate.

**ARIA attributes:** Components must be accessible:
```pug
section(role="region" aria-labelledby="section-title")
  h2#section-title About This Location
  button(
    aria-controls="details-panel"
    :aria-expanded="isExpanded"
    @click="toggleDetails"
  ) Show More
```

**Conditional `aria-labelledby`:** When an ARIA attribute depends on a reactive value that may be `null`, use `v-bind` with a ternary to avoid rendering `="null"` in the DOM:
```pug
section(v-bind="headingId ? { 'aria-labelledby': headingId } : {}")
```

**Parent-child lifecycle communication (`@ready` pattern):** Parents must NOT listen to child lifecycle hooks (e.g., `@hook:mounted`). To signal readiness, children emit a custom event and parents listen to it. The `@hook:` pattern is fragile and relies on Vue internals:
```javascript
// Child component
mounted() {
  this.$emit('ready');
},

// Parent template
//- child-component(@ready="handleChildReady")
```

**Keyboard navigability:** Interactive non-native elements must include `tabindex="0"` and keyboard handlers:
```pug
.thumbnail(
  role="button"
  tabindex="0"
  @click="selectImage(img)"
  @keydown.enter.prevent="selectImage(img)"
  @keydown.space.prevent="selectImage(img)"
)
```

**Event tracking:** Use `this.trackMREvent(eventName, properties)` for all meaningful user interactions (CTA clicks, link clicks, gallery views). Use `this.trackMREventAndRedirect(eventName, url)` for tracking + navigation in a single call. Both methods are globally available via mixin — **no import needed**.

**Tracking rules:**
- **Exclusively** use `this.trackMREvent()` or `this.trackMREventAndRedirect()` — never use direct `segmentTracking` or `window.analytics` calls
- For page load events, call `trackMREvent` in a `watch` with `immediate: true` on the relevant data
- `trackMREvent` does not require import statements or mixin declarations

**Date handling:** Always use `dayjs`:
```javascript
import dayjs from '@dayjs';

// In computed (not data):
computed: {
  todayDayName() {
    return dayjs().format('dddd');
  },
},
```

---

### 11. Critical Do's and Don'ts

**DO:**
- Declare `emits` explicitly on every component
- Use `ImgBox` for all images (never plain `<img>`)
- Use optional chaining (`?.`) — never legacy `getObjProperty()`
- Use scoped Stylus styles with design system variables
- Use path aliases for cross-directory imports
- Always use brackets for if/else — no one-line conditionals
- Extract module-level constants outside `export default` — no magic numbers
- Alphabetize computed properties and style block properties
- Use semantic HTML tags (`section`, `main`, `dl`)
- Add ARIA attributes and keyboard handlers to interactive elements
- Use native `<button>` for clickable actions — not `<a>` without `href`
- Use `this.trackMREvent()` or `this.trackMREventAndRedirect()` for all analytics events
- Import `dayjs` directly (`import dayjs from '@dayjs'`), use in computed
- Use `window.matchMedia` for responsive viewport detection
- Add `v-if` guards on all `ImgBox` instances where data may be null
- Add `// TODO:` comments on placeholder UI referencing ticket numbers
- Aggressively remove unused variables, props, methods, computed, data

**DON'T:**
- Don't create new mixins — use composables
- Don't use `ResponsiveImage` — use `ImgBox`
- Don't mount modals with `v-if` — use `modal/showModal` dispatch
- Don't hardcode hex colors in styles — use design system variables
- Don't use approximate breakpoint variables — if no exact match, keep hardcoded `px`
- Don't use relative imports across directories — use `@aliases`
- Don't use `console.*` without `// eslint-disable-next-line no-console`
- Don't overuse `:deep()` — prefer props for child component styling; use `:deep()` only for simple targeted overrides
- Don't call `segmentTracking` or `window.analytics` directly — use `trackMREvent`
- Don't use `this.$dayjs` — import dayjs directly
- Don't use `window.resize` listeners for responsive logic — use `window.matchMedia`
- Don't put complex inline logic in template event handlers — extract to methods
- Don't nest interactive elements (e.g., `<a>` inside `role="button"`)
- Don't define `window.matchMedia` at module level — it breaks SSR (store in `data`/`mounted`)
- Don't use `@hook:mounted` to listen to child lifecycle — emit a custom event instead
