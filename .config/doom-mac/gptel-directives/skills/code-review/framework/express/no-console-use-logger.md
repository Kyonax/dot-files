---
id: rule-fw-exp-001
title: Use Logger Module Instead of Console
severity: MEDIUM
tags: console, logger, error, structured, log
---

Use the project's logger module for all logging; never use `console.log`, `console.error`, or `console.warn` in application code.

### Apply
- All server-side route handlers, middleware, and service modules
- Shared library code that runs on the server

### Skip
- CLI scripts, build tools, and development-only utilities
- Client-side browser code where the logger module is unavailable

### Bad
```javascript
const express = require('express');
const router = express.Router();

router.get('/api/products', async (req, res) => {
  try {
    const products = await fetchProducts();
    console.log('Fetched products:', products.length);
    res.json(products);
  } catch (error) {
    console.error('Failed to fetch products:', error);
    res.status(500).json({ error: 'Internal error' });
  }
});
```

### Good
```javascript
const express = require('express');
const logger = require('logger');
const router = express.Router();

router.get('/api/products', async (req, res) => {
  try {
    const products = await fetchProducts();
    logger.info('Fetched products', { count: products.length });
    res.json(products);
  } catch (error) {
    logger.error('Failed to fetch products', { error: error.message, stack: error.stack });
    res.status(500).json({ error: 'Internal error' });
  }
});
```

### Edge
CLI scripts and build tools are exceptions where `console.log` is acceptable, since they run outside the application server context. If `console` must be used in application code for a specific reason, suppress the lint rule with an inline disable comment and explain why.
