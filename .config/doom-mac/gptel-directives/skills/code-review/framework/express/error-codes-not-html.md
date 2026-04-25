---
id: rule-fw-exp-002
title: Return Structured Error Codes, Not HTML
severity: MEDIUM
tags: error, response, code, html, status, json
---

API endpoints must return structured error responses (JSON with status, code, and message), never HTML error pages.

### Apply
- All REST API endpoints
- Error handlers and middleware that serve API routes

### Skip
- Server-rendered page routes that intentionally return HTML error pages to browsers
- Health check endpoints that return plain text

### Bad
```javascript
router.get('/api/orders/:id', async (req, res) => {
  const order = await getOrder(req.params.id);
  if (!order) {
    res.status(404).send('<h1>Not Found</h1><p>Order does not exist.</p>');
    return;
  }
  res.json(order);
});

// Generic error handler returning HTML
app.use((err, req, res, next) => {
  res.status(500).send('<h1>Server Error</h1>');
});
```

### Good
```javascript
router.get('/api/orders/:id', async (req, res) => {
  const order = await getOrder(req.params.id);
  if (!order) {
    res.status(404).json({
      status: 404,
      code: 'ORDER_NOT_FOUND',
      message: 'The requested order does not exist.',
    });
    return;
  }
  res.json(order);
});

// Generic error handler returning JSON
app.use((err, req, res, next) => {
  res.status(500).json({
    status: 500,
    code: 'INTERNAL_ERROR',
    message: 'An unexpected error occurred.',
  });
});
```

### Edge
Some APIs serve both HTML pages and JSON endpoints. In those cases, check the `Accept` header or use a path prefix convention (e.g., `/api/`) to determine the response format. The rule applies specifically to routes consumed by API clients.
