---
title: Testing Standards — Vitest, Vue Test Utils, and Mocking Patterns
impact: HIGH
impactDescription: Defines how all frontend tests are written and structured, ensuring reliable test coverage and consistent mocking patterns.
tags: testing, vitest, jest, vue-test-utils, test, mock, shallowMount, createStore, co-located, setup, vi, nextTick, stubbed, assertions
---

## Testing Standards — Vitest, Vue Test Utils, and Mocking Patterns

Frontend tests use **Vitest** with **Vue Test Utils**. Server-side tests use **Jest**. Tests are co-located with their source files.

---

### 1. Test File Location and Naming

Tests live **next to the component they test**:

```
ComponentName/
├── ComponentName.vue
├── ComponentName.test.js      # Co-located test file
└── index.js
```

**Naming:** `ComponentName.test.js` — matches the component file name.

---

### 2. Running Tests

```bash
cd website && npm run test:vue                    # All frontend Vue tests (Vitest)
cd website && npm run test:vue {component_name}   # Single component (e.g., npm run test:vue HairColorBarLocationHeroV2)
npm run unit-tests                                # Server-side tests (Jest)
```

**Config location:** `website/vite/vite.dev.config.mjs` (test section)

```javascript
test: {
  globals: true,
  environment: 'happy-dom',
  setupFiles: ['vitest.setup.js'],
  include: ['**/src/vuescripts/**/*.test.js'],
}
```

**Required environment variables:** `TZ=UTC NODE_ENV=test`

---

### 3. Test Setup File (`vitest.setup.js`)

The setup file pre-mocks common external modules so tests don't make real API calls:

**Pre-mocked modules:**
- `@services/mrApi` — API client
- `axios` — HTTP client
- `@sentry/browser` — Error tracking
- `@stripe/stripe-js` — Payment
- `mailcheck`, `twilio-video`, `@utilities/loadScript`

**Global stubs** (auto-applied to all tests):
- `mr-icon`, `CmsPartialSsr`, `svgicon` — Custom components
- `transition`, `transition-group` — Vue transitions

**Browser API mocks:**
- `IntersectionObserver`
- `matchMedia`
- `window.scrollTo`

---

### 4. Component Test Pattern

```javascript
import { shallowMount } from '@vue/test-utils';
import { createStore } from 'vuex';
import MyComponent from './MyComponent.vue';

describe('MyComponent', () => {
  const createWrapper = (propsOverrides = {}, stateOverrides = {}) => {
    const store = createStore({
      modules: {
        modal: {
          namespaced: true,
          state: { visible: false, ...stateOverrides.modal },
          actions: { showModal: vi.fn(), hideModal: vi.fn() },
        },
        global: {
          namespaced: true,
          state: { isMobile: false, ...stateOverrides.global },
        },
      },
    });

    return shallowMount(MyComponent, {
      props: { title: 'Test', ...propsOverrides },
      global: {
        plugins: [store],
        mocks: {
          trackMREvent: vi.fn(),
          $mrApi: { get: vi.fn(), post: vi.fn() },
        },
        stubs: {
          'focus-lock': { template: '<div><slot /></div>' },
        },
      },
    });
  };

  it('renders the title', () => {
    const wrapper = createWrapper({ title: 'Hello' });
    expect(wrapper.find('h2').text()).toBe('Hello');
  });

  it('dispatches showModal on click', async () => {
    const wrapper = createWrapper();
    await wrapper.find('.open-btn').trigger('click');
    expect(wrapper.vm.$store.dispatch).toHaveBeenCalledWith(
      'modal/showModal',
      expect.objectContaining({ component: 'ConfirmModal' })
    );
  });
});
```

---

### 5. Snapshot Testing — FORBIDDEN

**Snapshot testing is explicitly forbidden.** Never use `toMatchSnapshot()` or `toMatchInlineSnapshot()`. All rendering checks must be explicit assertions.

**Instead of snapshots, assert specific DOM output:**
```javascript
// WRONG — snapshot
it('renders correctly when loading', () => {
  const wrapper = createWrapper({ loading: true });
  expect(wrapper.html()).toMatchSnapshot(); // FORBIDDEN
});

// CORRECT — explicit assertions
it('shows spinner when loading', () => {
  const wrapper = createWrapper({ loading: true });
  expect(wrapper.findComponent({ name: 'MrSpinnerVeil' }).exists()).toBe(true);
  expect(wrapper.find('.content-area').exists()).toBe(false);
});
```

---

### 5.1 Stubbed Component Interactions

When using `shallowMount`, child components are stubbed. **Do not** assert text content or internal rendering of stubbed children. Instead, assert:

- **Existence:** `.exists()` — the stub is rendered
- **Props passed:** `.props()` — correct data flows down
- **Emitted events:** `.emitted()` — parent reacts correctly
- **Parent state:** Assert the parent's data/computed that control the stub

```javascript
// WRONG — asserting text inside a stubbed child
expect(wrapper.findComponent(ChildComponent).text()).toBe('Hello');

// CORRECT — assert the stub exists and received the right props
const child = wrapper.findComponent(ChildComponent);
expect(child.exists()).toBe(true);
expect(child.props('title')).toBe('Hello');
```

---

### 5.2 Asynchronous Updates in Tests

**Always use `await wrapper.vm.$nextTick()`** after triggering state changes (method calls, data assignments) that cause DOM updates, before asserting the rendered output.

```javascript
it('shows desktop layout when not mobile', async () => {
  const wrapper = createWrapper();
  wrapper.vm.isMobile = false;
  await wrapper.vm.$nextTick();
  expect(wrapper.find('.main-display').exists()).toBe(true);
  expect(wrapper.findComponent(MobileLayout).exists()).toBe(false);
});
```

---

### 6. Mocking `window.matchMedia`

Components that use `window.matchMedia` for responsive detection require special mocking. The key challenge: if `matchMedia` is called at module-level (`const query = window.matchMedia(...)` outside `export default`), it executes at import time — before `beforeEach` stubs are applied. The solution is to either move the call to `mounted`/`data` in the component, or pre-define the mock object.

**Recommended pattern:**
```javascript
describe('MyResponsiveComponent', () => {
  let mockMediaQueryObject;

  beforeEach(() => {
    mockMediaQueryObject = {
      matches: false,
      addEventListener: vi.fn(),
      removeEventListener: vi.fn(),
    };
    vi.stubGlobal('matchMedia', vi.fn().mockReturnValue(mockMediaQueryObject));
  });

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  test('mounted: adds a change event listener', () => {
    const wrapper = createWrapper();
    expect(mockMediaQueryObject.addEventListener).toHaveBeenCalledWith(
      'change',
      wrapper.vm.handleMediaQueryChange
    );
  });

  test('beforeUnmount: removes the change event listener', () => {
    const wrapper = createWrapper();
    const handler = wrapper.vm.handleMediaQueryChange;
    wrapper.unmount();
    expect(mockMediaQueryObject.removeEventListener).toHaveBeenCalledWith(
      'change',
      handler
    );
  });

  test('sets isMobile when media query matches', () => {
    const wrapper = createWrapper();
    wrapper.vm.handleMediaQueryChange({ matches: true });
    expect(wrapper.vm.isMobile).toBe(true);
  });
});
```

**Key insight:** Assert directly against `mockMediaQueryObject.addEventListener` — do not try to fish results from `matchMediaMock.mock.results[0].value`, as this fails when the component stores the query in `data`/`mounted` rather than at module level.

---

### 6.1 Mocking the Centralized `global/isDesktop` Getter

For components that use `mapGetters('global', ['isDesktop'])`, create a store mock with a configurable second parameter:

```javascript
const createMockStore = (moduleState = {}, isDesktop = true) => {
  return createStore({
    modules: {
      myModule: {
        namespaced: true,
        state: { ...moduleState },
        actions: { loadData: vi.fn() },
      },
      global: {
        namespaced: true,
        getters: { isDesktop: () => isDesktop },
      },
    },
  });
};

// Usage:
it('shows FixedCtaBar when not desktop', () => {
  const wrapper = createWrapper({}, false); // isDesktop = false
  expect(wrapper.findComponent(FixedCtaBar).props('visible')).toBe(true);
});
```

---

### 7. Testing Computed Properties (Without Mounting)

For unit-testing computed properties directly:

```javascript
import Component from './Component.vue';

it('computes isActive correctly', () => {
  const localThis = { modelValue: 5, disabled: false };
  const result = Component.computed.isActive.call(localThis);
  expect(result).toBe(true);
});
```

---

### 8. Mocking API Services

```javascript
import vueCartSvc from '@services/vueCartSvc';

vi.mock('@services/vueCartSvc', () => ({
  default: {
    getCartDataForView: vi.fn().mockResolvedValue({ data: { items: [] } }),
    addToCart: vi.fn().mockResolvedValue({ data: { success: 1 } }),
  },
}));
```

---

### 9. Key Testing Conventions

**DO:**
- Create a `createWrapper` factory function in each test file
- Mock Vuex stores with `createStore()` — provide only the modules your component uses
- Mock global mixins methods (`trackMREvent`, `scrollTo`, `goToPath`) in `global.mocks`
- Use `shallowMount` by default — `mount` only when testing child component integration
- Use `await wrapper.vm.$nextTick()` after every state change before asserting DOM
- Assert stubbed children via `.exists()`, `.props()`, `.emitted()` — not `.text()`
- Run tests before creating PRs: `cd website && npm run test:vue`
- Use `console.warn` for analytics warnings when Segment is unavailable in test context

**DON'T:**
- **Don't use snapshot tests** — `toMatchSnapshot()` is forbidden
- Don't let tests make real API calls — everything should be mocked
- Don't assert text content or internal rendering of stubbed child components
- Don't import the full store — create minimal mock stores per test
- Don't skip mocking global mixins — they're injected into every component
- Don't forget `$nextTick` after triggering reactive state changes

---

### 10. Emit Before Redirect Pattern

When testing methods that call `trackMREventAndRedirect`, verify that `$emit` fires **before** the redirect call. The redirect may navigate the page away, preventing subsequent code from executing:

```javascript
it('emits cta-click before tracking redirect', async () => {
  const wrapper = createWrapper();
  await wrapper.find('.cta-btn').trigger('click');

  // Verify emit fires first
  expect(wrapper.emitted('cta-click')).toHaveLength(1);

  // Then verify tracking
  expect(wrapper.vm.trackMREventAndRedirect).toHaveBeenCalledWith(
    'Event Name',
    '/expected/url'
  );
});
```
