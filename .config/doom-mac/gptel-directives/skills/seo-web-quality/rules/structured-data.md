---
title: Implement Structured Data (JSON-LD)
impact: MEDIUM
impactDescription: Enhances search result appearance with rich snippets, improving click-through rates.
tags: seo, structured data, json-ld, schema, rich snippets
---

Structured data provides explicit information about a page's content, enabling search engines to display it as a "rich result" (e.g., with ratings, prices, or images). The preferred format is JSON-LD.

## 1. Common Schema Types

### Product
```html
<script type="application/ld+json">
{
  "@context": "https://schema.org",
  "@type": "Product",
  "name": "Blue Widget Pro",
  "image": "https://example.com/blue-widget.jpg",
  "description": "Premium blue widget with advanced features.",
  "offers": {
    "@type": "Offer",
    "price": "49.99",
    "priceCurrency": "USD",
    "availability": "https://schema.org/InStock"
  },
  "aggregateRating": {
    "@type": "AggregateRating",
    "ratingValue": "4.8",
    "reviewCount": "1250"
  }
}
</script>
```

### Article
```html
<script type="application/ld+json">
{
  "@context": "https://schema.org",
  "@type": "Article",
  "headline": "How to Choose the Right Widget",
  "author": {
    "@type": "Person",
    "name": "Jane Smith"
  },
  "datePublished": "2024-01-15"
}
</script>
```

### FAQ Page
```html
<script type="application/ld+json">
{
  "@context": "https://schema.org",
  "@type": "FAQPage",
  "mainEntity": [{
    "@type": "Question",
    "name": "What colors are available?",
    "acceptedAnswer": {
      "@type": "Answer",
      "text": "Our widgets come in blue, red, and green."
    }
  }]
}
</script>
```

### Breadcrumbs
```html
<script type="application/ld+json">
{
  "@context": "https://schema.org",
  "@type": "BreadcrumbList",
  "itemListElement": [{
    "@type": "ListItem",
    "position": 1,
    "name": "Products",
    "item": "https://example.com/products"
  },{
    "@type": "ListItem",
    "position": 2,
    "name": "Blue Widgets"
  }]
}
</script>
```

## 2. Validation
Always validate your structured data to ensure it's free of errors.

*   **Google Rich Results Test:** [search.google.com/test/rich-results](https://search.google.com/test/rich-results)
*   **Schema.org Validator:** [validator.schema.org](https://validator.schema.org/)
