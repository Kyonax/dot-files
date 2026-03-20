---
title: Express Routing — Endpoints, Views, and Middleware Patterns
impact: HIGH
impactDescription: Defines how all Express routes, API endpoints, and middleware are structured, critical for adding new pages or API methods.
tags: express, routing, endpoints, views, middleware, before, api, rest, controller, webservices, madison, node
---

## Express Routing — Endpoints, Views, and Middleware Patterns

The MR website runs on Express. Routes are organized into a three-file pipeline: pre-processing → API endpoints → page views.

---

### 1. Routing Pipeline

```
routing.js (orchestrator)
    ↓
before.js → endpoints.js → views.js
```

| File           | Purpose                                                          | Location                           |
|----------------|------------------------------------------------------------------|------------------------------------|
| `routing.js`   | Imports and chains the three route layers                        | `website/src/routing/routing.js`   |
| `before.js`    | Pre-processing middleware — query params, promo codes, referrals | `website/src/routing/before.js`    |
| `endpoints.js` | REST API routes (`/api/:module/:method`)                         | `website/src/routing/endpoints.js` |
| `views.js`     | Page routes — Pug renders, Vue SSR, redirects (~60KB)            | `website/src/routing/views.js`     |

---

### 2. Pre-Processing (`before.js`)

Runs before any route handler. Extracts and processes query parameters:

- **Promo codes:** `?offerCode=SAVE20` → auto-applies to cart
- **Referral campaigns:** `?campaignid=...` → tracks attribution
- **Advisor IDs:** `?advisorId=...` → associates with session
- **Extole sharing:** `?extole_share_channel=...`
- Uses `async.autoInject()` for ordered async processing

---

### 3. API Endpoints (`endpoints.js`)

**Primary pattern:** `/api/:module/:method` → dynamic service dispatch

```javascript
// Pattern: dynamically loads webservices[module][method]
exports.get = function(req, res) {
  callRestService(req, res, req.query);
};

exports.post = function(req, res) {
  callRestService(req, res, req.body);
};
```

**Modular endpoint files** (`website/src/routing/endpoints/`):
- `cart.js` — Cart operations (add, update, remove)
- `user.js` — User/customer operations
- `color-advisor.js` — Color advisor logic
- `contact.js` — Contact form submissions
- `support.js` — Support ticket creation
- `reward.js` — Rewards program
- `restAPI.js` — Generic REST handler

**Endpoint handler pattern:**
```javascript
endpoints.add = function(req, res) {
  cartSvc.addProduct(req.body, req, function(err, product) {
    if (err) {
      res.send({ success: 0, err: err.message });
    } else {
      res.send({ ...product, success: 1 });
    }
  });
};
```

**Key patterns:**
- Callback-based async (legacy — not Promise/async-await)
- Controllers loaded via `require('controllers')` (NODE_PATH resolution)
- Response format: `{ success: 0/1, err: "message", data: ... }`
- Rate limiting and auth validation applied per endpoint

---

### 4. Page Views (`views.js`)

Handles all page rendering — both legacy Pug and modern Vue SSR.

**Legacy Pug render:**
```javascript
app.get('/about', function(req, res) {
  res.render('about', { pageTitle: 'About Us', config: config });
});
```

**Vue SSR render:**
```javascript
app.get('/cart', function(req, res) {
  // CMS module handles Vue SSR rendering
  // See ssr-architecture.md for the full pipeline
});
```

**Route with experiment branching:**
```javascript
{
  path: '/cart',
  component: () => {
    if (window.experiments?.['Cart Redesign'] === 'B') {
      return import(/* webpackChunkName: "cart-v2" */ '../CartV2');
    }
    return import(/* webpackChunkName: "cart" */ '../Cart');
  }
}
```

**Route guards** (Vue Router):
```javascript
export function requiresLogin(to, from, next) {
  vueCustomerSvc.getLoggedInInfo().then(async (res) => {
    const loggedIn = res?.data?.loggedIn;
    if (!loggedIn) {
      window.location = notLoggedInLocation(to.fullPath);
      return next(false);
    }
    next();
  }).catch(() => {
    window.location = notLoggedInLocation(to.fullPath);
    return next(false);
  });
}
```

---

### 5. Module Resolution

Controllers, models, and shared utilities are accessed via `NODE_PATH=mr_modules/`:

```javascript
const controllers = require('controllers');   // mr_modules/controllers/
const config = require('config');             // mr_modules/config/
const dataAccess = require('dataAccess');     // mr_modules/dataAccess/
const MRUtils = require('MRUtils');           // mr_modules/MRUtils/
```

No relative paths needed for `mr_modules/` — `NODE_PATH` makes them available as top-level requires.
