---
title: Dynamic Yield Integration & Development
impact: CRITICAL
impactDescription: Ensures consistent and correct implementation of Dynamic Yield personalization, which is critical for campaign performance, data accuracy, and site stability.
tags: dynamic-yield, dy, personalization, api, javascript, campaign, template, a/b testing, machine-learning
---

### Overview
This rule governs the integration and development of Dynamic Yield (Experience OS) within Madison Reed's frontend. It provides the architectural overview, API specifications, and mandatory best practices for building, deploying, and maintaining personalized experiences. Adherence is crucial for data integrity, campaign performance, and preventing conflicts with the core application.

### 1. Core Concepts
-   **Campaigns:** The live, functional containers that organize and serve content. They define the **targeting logic** (who, where, when) and **allocation strategy** (A/B testing, AI optimization). They are triggered by an **API Selector Name**.
-   **Templates:** Reusable code blueprints (HTML, CSS, JS, JSON) that define the structure and variables for content. A single template change propagates to all linked variations.
-   **Variations:** The specific content instances within a campaign, created from a template and populated with dynamic data (text, images, links).
-   **Page Context:** Critical signals (`HOMEPAGE`, `PRODUCT`, `CART`) that tell DY where the user is, triggering the correct campaign targeting.
-   **User Identification:**
    -   **DYID (Dynamic Yield ID):** A unique identifier for a specific device/browser.
    -   **CUID (Customer Unique ID):** A persistent, cross-device identifier (e.g., hashed email or CRM ID). Reporting a CUID merges the device's history with the user's master profile.

### 2. Implementation Paradigm
Madison Reed primarily uses the **Server-Side Experience API**, a REST-based framework (HTTPS/JSON). This method is preferred for its high performance, security, and flicker-free rendering.

### 3. Primary API: The `/v2/serve/user/choose` Endpoint
This is the central decision engine for fetching personalized experiences.
-   **Method:** `POST`
-   **Endpoint (US):** `https://dy-api.com/v2/serve/user/choose`
-   **Authentication:** API Key in the request headers.
-   **Request Payload:**
    -   `selector`: (String) The **Selector Name** of the campaign being requested.
    -   `user`: (Object) Contains the `dyid` and/or `cuid`.
    -   `session`: (String) Current session identifier.
    -   `context`: (Object) Environmental signals including `page` (type, location, etc.) and `device`.
-   **Response Payload:** A JSON object whose structure is defined by the campaign's **Template**, populated with the winning **Variation's** specific values.

### 4. Behavioral Event Tracking
Events fuel the AdaptML machine learning engine. Use the standard `dyType` strings for accurate processing.
-   **`purchase-v1`**: Requires `value` and `cart` array.
-   **`add-to-cart-v1`**
-   **`identify-v1`**: Used to map a CUID to a DYID.
-   **`custom-event-v1`**: Flexible schema for unique, non-standard actions.

### 5. Template Development & Modification Guidelines
Modifying templates requires strict adherence to the following to ensure stability and performance.

#### 5.1 Global Impact & Lifecycle
-   **Universal Propagation:** An edit to a template **automatically updates every linked variation** across all campaigns in that section. Exercise extreme caution.
-   **Version Control:** Every change is versioned in the Activity Logs (Date, User, Changes). Use this to troubleshoot or revert.
-   **Unlinking vs. Deleting:** Unlinking a variation from a template preserves its current code but severs it from future template updates. Deleting a template is permanent.

#### 5.2 Variable Logic & Syntax
-   **Placeholders:** Use the `${variable_name}` syntax to define injection points for dynamic content.
-   **Recommendation Loops:** For API Recommendation campaigns, only a **single recommendation loop** (e.g., `${#recommendations}`) is permitted within the HTML tab.
-   **Sync Behavior:** In the DY UI, removing a variable from the HTML/CSS/JS tabs is required before it can be removed from the JSON schema.

#### 5.3 Technical Constraints & Performance
-   **Line Length Limit:** Code lines **must not exceed 10,000 characters**. Long strings or minified blocks must be broken into smaller segments to prevent variable processing failures.
-   **Forbidden Keywords:** In API templates, **do not use `DY.` or `DYO.` prefixes** in your code. These are reserved for client-side script implementations and will cause conflicts.
-   **Minification:** **Always minify** template code (HTML, CSS, JS) before saving to optimize loading times and reduce the script footprint.
-   **Resource Efficiency:** Using templates is mandatory over "No Template" variations. The template code is loaded once, while variations only contribute their variable values, significantly reducing final payload size.

#### 5.4 API Integration & Caching
-   **Dynamic Tokens:** Saving a template generates a new, unique `template.token`. If your application caches template assets, you **must invalidate the cache** when the token in the `/choose` response changes.
-   **Data Center Consistency:** Ensure any template fetch URLs match your account’s region (`.com` for US, `.eu` for EU).

#### 5.5 Deployment Safety & Best Practices
-   **Conflict Prevention:** Always perform thorough QA on modified templates to ensure new CSS or JS doesn't conflict with existing site styles or global scripts.
-   **Fallback Logic:** Your application code **must handle missing or null variable values gracefully** to prevent rendering broken UI components.

### 6. Shopping Muse (Generative AI)
For conversational commerce implementations, the Shopping Muse API interprets natural language queries to return intent-driven product recommendations. (Further documentation required for specific integration details).
