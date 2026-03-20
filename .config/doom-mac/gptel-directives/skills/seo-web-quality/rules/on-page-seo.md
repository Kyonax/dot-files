---
title: Apply On-Page SEO Elements
impact: HIGH
impactDescription: Optimizes individual page content to improve search rankings and user experience.
tags: seo, on-page, content, title, meta, description, headings, images, links
---

## 1. Title Tags

The `<title>` tag is a strong signal of a page's topic. It appears in the browser tab and search results.

**Bad:** `<title>Home</title>`
**Good:** `<title>Blue Widgets for Sale | Premium Quality | Example Store</title>`

**Guidelines:**
- Keep it under 60 characters.
- Place the primary keyword near the beginning.
- Make it unique for every page.

## 2. Meta Descriptions

The `<meta name="description">` tag provides a summary of the page's content for search results. It doesn't directly impact ranking but heavily influences click-through rate.

**Bad:** `<meta name="description" content="">`
**Good:** `<meta name="description" content="Shop premium blue widgets with free shipping and 30-day returns. Rated 4.9/5 by over 10,000 customers. Order today!">`

**Guidelines:**
- Keep it around 150-160 characters.
- Make it a compelling call-to-action.
- Ensure it's unique and relevant to the page.

## 3. Heading Structure

Headings (`<h1>`, `<h2>`, etc.) create a logical hierarchy for your content, which helps both users and search engines understand its structure.

**Bad Structure:**
```html
<h2>Welcome</h2>
<h4>Products</h4>
<h1>Contact</h1>
```

**Good Structure:**
```html
<h1>Premium Blue Widgets</h1>
  <h2>Product Features</h2>
    <h3>Durability</h3>
  <h2>Customer Reviews</h2>
```

**Guidelines:**
- Use exactly one `<h1>` per page for the main topic.
- Do not skip heading levels (e.g., `<h1>` to `<h3>`).

## 4. Image SEO

Optimize images to improve page speed and provide context to search engines.

**Bad:** `<img src="IMG_123.jpg">`
**Good:** `<img src="blue-widget-side-view.webp" alt="A premium blue widget with a chrome finish." width="800" height="600" loading="lazy">`

**Guidelines:**
- Use descriptive filenames (e.g., `blue-widget.jpg`).
- Write descriptive `alt` text for accessibility and SEO.
- Use modern formats like WebP or AVIF.
- Lazy-load images that are below the fold.

## 5. Internal Linking

Linking to other relevant pages on your site helps search engines understand your site's structure and distributes page authority.

**Bad:** `<a href="/products">Click here</a>`
**Good:** `<a href="/products/blue-widgets">Browse our collection of blue widgets</a>`

**Guidelines:**
- Use descriptive anchor text that includes keywords.
- Link to relevant, important pages.
