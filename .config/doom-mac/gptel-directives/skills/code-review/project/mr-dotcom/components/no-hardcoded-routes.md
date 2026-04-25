---
id: rule-pj-mrd-055
title: No Hardcoded Routes in Reusable Components
severity: MEDIUM
tags: route, hardcoded, path, reusable, $route
---

Reusable components must not contain hardcoded route paths; pass routes as props or use a routes config.

### Apply
- Any component intended for reuse across multiple pages or contexts

### Skip
- Page-level components that are inherently tied to a single route

### Bad
```javascript
<template lang="pug">
.service-card
  a(href="/hair-color-bar") Find a Location
  MrBtn(@click="$router.push('/booking/select-service')") Book Now
</template>
```

### Good
```javascript
<template lang="pug">
.service-card
  a(:href="locationUrl") Find a Location
  MrBtn(@click="$router.push(bookingUrl)") Book Now
</template>

// Props:
props: {
  locationUrl: { type: String, required: true },
  bookingUrl: { type: String, required: true },
},
```

### Edge
For components that always link to the same destination but are used site-wide (e.g., a global footer), a centralized routes config imported by the component is acceptable instead of props.
