---
title: Dynamic Yield Integration — Personalization, A/B Testing, and Experience API
impact: CRITICAL
impactDescription: Ensures correct implementation of Dynamic Yield personalization, critical for campaign performance, data accuracy, and site stability.
tags: dynamic-yield, dy, personalization, api, campaign, template, variation, a/b-testing, experience-api, choose, cuid, dyid, event-tracking, shopping-muse, server-side
---

## Dynamic Yield Integration — Personalization, A/B Testing, and Experience API

Dynamic Yield (Experience OS) is the primary engine for personalization and A/B testing at Madison Reed. It decouples marketing content from the application's release cycle. Developers build **Templates**; business teams launch **Campaigns** using those templates.

---

### 1. Core Concepts

| Concept          | Definition                                                                                                                                    |
|------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| **Campaign**     | Live container with targeting logic (who, where, when) and allocation strategy (A/B, AI optimization). Triggered by an **API Selector Name**. |
| **Template**     | Reusable code blueprint (HTML, CSS, JS, JSON) defining structure and variables. A single template change propagates to all linked variations. |
| **Variation**    | Specific content instance within a campaign, created from a template, populated with dynamic data.                                            |
| **Page Context** | Signals (`HOMEPAGE`, `PRODUCT`, `CART`) telling DY where the user is, triggering correct targeting.                                           |
| **DYID**         | Device/browser identifier (Dynamic Yield ID).                                                                                                 |
| **CUID**         | Customer Unique ID — persistent, cross-device (e.g., hashed email). Reporting CUID merges device history with the user's master profile.      |

---

### 2. Implementation: Server-Side Experience API

Madison Reed uses the **Server-Side Experience API** (REST, HTTPS/JSON) — preferred for performance, security, and flicker-free rendering.

### 3. Primary API: `/v2/serve/user/choose`

The central decision engine for fetching personalized experiences.

- **Method:** `POST`
- **Endpoint (US):** `https://dy-api.com/v2/serve/user/choose`
- **Authentication:** API Key in headers

**Request payload:**
```json
{
  "selector": "campaign-selector-name",
  "user": { "dyid": "...", "cuid": "..." },
  "session": "session-id",
  "context": {
    "page": { "type": "HOMEPAGE", "location": "/" },
    "device": "desktop"
  }
}
```

**Response:** JSON structure defined by the campaign's Template, populated with the winning Variation's values.

---

### 4. Event Tracking

Events fuel the AdaptML machine learning engine. Use standard `dyType` strings:

| dyType            | Purpose              | Required Fields       |
|-------------------|----------------------|-----------------------|
| `purchase-v1`     | Purchase completed   | `value`, `cart` array |
| `add-to-cart-v1`  | Item added to cart   | Product details       |
| `identify-v1`     | Map CUID to DYID     | User identifiers      |
| `custom-event-v1` | Non-standard actions | Flexible schema       |

---

### 5. Template Development Guidelines

#### 5.1 Global Impact & Lifecycle
- **Universal propagation:** Editing a template updates **every linked variation** across all campaigns. Exercise extreme caution.
- **Version control:** Every change is versioned in Activity Logs (Date, User, Changes).
- **Unlinking vs. Deleting:** Unlinking preserves current code but severs from future updates. Deleting is permanent.

#### 5.2 Variable Logic & Syntax
- **Placeholders:** `${variable_name}` syntax for dynamic content injection.
- **Recommendation loops:** Only a **single** `${#recommendations}` loop permitted per HTML tab.
- **Sync behavior:** Remove a variable from HTML/CSS/JS tabs before removing from JSON schema.

#### 5.3 Technical Constraints
- **Line length limit:** Code lines **must not exceed 10,000 characters**.
- **Forbidden keywords:** Never use `DY.` or `DYO.` prefixes in API templates — these are reserved for client-side scripts.
- **Minification:** Always minify template code (HTML, CSS, JS) before saving.
- **Templates mandatory:** Always use templates over "No Template" variations — template code loads once, variations only contribute variable values.

#### 5.4 API Integration & Caching
- **Dynamic tokens:** Saving a template generates a new `template.token`. Invalidate cache when token changes.
- **Data center consistency:** Ensure fetch URLs match your account's region (`.com` for US, `.eu` for EU).

#### 5.5 Deployment Safety
- **Conflict prevention:** QA modified templates to ensure CSS/JS doesn't conflict with site styles or global scripts.
- **Fallback logic:** Application code **must handle missing or null variable values gracefully** to prevent broken UI.

---

### 6. Shopping Muse (Generative AI)

For conversational commerce: Shopping Muse API interprets natural language queries to return intent-driven product recommendations. (Further documentation required for specific integration details.)
