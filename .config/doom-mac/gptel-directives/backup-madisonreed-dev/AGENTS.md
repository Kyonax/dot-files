# Madison Reed Development Best Practices

**Version 1.2.0**
Kyonax
August 2024

## Abstract
A comprehensive guide to Madison Reed's frontend development standards for AI agents. This document details the core philosophy behind the company's utility-first CSS architecture, covering Vue, Pug, and Stylus. It explains the "why" behind the strict adherence to a shared design system, emphasizing the importance of consistency, maintainability, and responsive design. This guide is the primary source for understanding the architectural principles that govern all frontend styling and layout.

## 1. Referenced Files
This guide provides the high-level strategy and rationale. For specific class lists and implementation examples, the following rule files **must** be consulted:

*   `./rules/spacing-utilities.md`
*   `./rules/typography-utilities.md`
*   `./rules/flexbox-layout.md`
*   `./rules/utility-classes.md`

---

## 2. Styling & UI
**Impact: CRITICAL**
Failure to follow these rules directly impacts the visual consistency, brand integrity, and overall user experience of Madison Reed's e-commerce platform. This is the highest priority category for frontend output. The core principle is **composition over custom CSS**: UI is built by combining predefined utility classes in Pug, not by writing new CSS in Stylus.

### 2.1 Spacing System (`spacing-utilities`)
**Impact: CRITICAL (ensures a consistent visual rhythm and simplifies responsive design)**
**When to Apply:** When implementing any `margin` or `padding` on any element.
**How it Works:** Madison Reed employs a systematic, scale-based spacing system to create a predictable and harmonious layout. Using classes like `.mt-100m` or `.sm-px-50m` instead of hard-coded values (`margin-top: 1em`) ensures that all spacing adheres to a deliberate design scale. This approach eliminates "magic numbers" and makes the responsive behavior of a component's spacing declarative and immediately obvious from the Pug template, rather than being hidden in media queries.

### 2.2 Typography & Color (`typography-utilities`)
**Impact: CRITICAL (maintains brand identity and ensures readability)**
**When to Apply:** When styling any text element, including its font, size, weight, and color.
**How it Works:** Typography and color are the voice of the brand. These utilities enforce strict adherence to brand guidelines. The responsive font size system (`.xs-f-medium`, `.lg-f-large`) ensures optimal readability on all devices. The color palette (`.color-mr-purple`) is a closed system; using these classes instead of hex codes ensures that brand-wide color updates can be made from a single source of truth. This system is designed for absolute brand consistency and long-term maintainability.

### 2.3 Flexbox & Layout (`flexbox-layout`)
**Impact: HIGH (promotes clean, maintainable, and self-documenting component architecture)**
**When to Apply:** When structuring any component or page layout.
**How it Works:** Flexbox utilities are the primary tool for building component structures. By using declarative classes like `.flex`, `.flex-col`, `.space-between`, and `.flex-1`, the layout's intent becomes self-documenting within the Pug template. This approach is superior to custom CSS because it creates highly modular, reusable components whose structure can be understood at a glance, without needing to cross-reference a separate stylesheet. It is the foundation of our component architecture.

### 2.4 General Utilities (`utility-classes`)
**Impact: HIGH (prevents code duplication and ensures consistency for common styles)**
**When to Apply:** For common, single-purpose styling needs like positioning, interactivity, and borders.
**How it Works:** This file contains a comprehensive "toolkit" of helper classes that handle common styling tasks not covered by the major systems. Utilities like `.clickable`, `.v-center`, `.border-radius-4`, and `.shake` prevent developers from writing small, one-off CSS snippets that lead to code duplication and inconsistency. This catalog ensures that there is a standardized, approved way to handle these common UI requirements, keeping the codebase clean and predictable.

### 2.5 Personalization & A/B Testing (`dynamic-yield`)
**Impact: CRITICAL (drives personalization, A/B testing, and revenue-critical user experiences)**
**When to Apply:** When creating, modifying, or debugging any component or logic served through the Dynamic Yield platform.
**How it Works:** Dynamic Yield serves as our primary engine for personalization and experimentation. It decouples marketing and business-driven content from the main application's release cycle. Our architectural approach is to treat DY as a headless system where developers are responsible for building robust, performant, and reusable **Templates**. The business teams then use these templates to build and launch **Campaigns**. This separation of concerns is paramount. The `dynamic-yield` rule file provides the comprehensive technical specifications to ensure that these templates are built safely, integrate flawlessly with our frontend via the Experience API, and do not introduce performance issues or conflicts with the core codebase. Adherence is non-negotiable to protect the stability and data integrity of our most critical user funnels.
