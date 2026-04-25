---
id: rule-u-sc-005
title: Extract Repeated Logic
severity: MEDIUM
tags: repeated, extract, helper, dry
---

When three or more identical (or near-identical) lines appear in different functions, extract them into a shared helper.

### Apply
- Duplicate blocks across methods, actions, or utility functions within the same module or file

### Skip
- Two occurrences that are unlikely to grow (the cost of abstraction outweighs the duplication)
- Superficially similar code that differs in meaningful ways (different error handling, different context)

### Bad
```javascript
function createUser(data) {
  const timestamp = Date.now();
  const id = generateId();
  const record = { ...data, id, createdAt: timestamp, updatedAt: timestamp };
  return db.insert('users', record);
}

function createOrder(data) {
  const timestamp = Date.now();
  const id = generateId();
  const record = { ...data, id, createdAt: timestamp, updatedAt: timestamp };
  return db.insert('orders', record);
}

function createInvoice(data) {
  const timestamp = Date.now();
  const id = generateId();
  const record = { ...data, id, createdAt: timestamp, updatedAt: timestamp };
  return db.insert('invoices', record);
}
```

### Good
```javascript
function buildRecord(data) {
  const timestamp = Date.now();
  const id = generateId();
  return { ...data, id, createdAt: timestamp, updatedAt: timestamp };
}

function createUser(data) {
  return db.insert('users', buildRecord(data));
}

function createOrder(data) {
  return db.insert('orders', buildRecord(data));
}

function createInvoice(data) {
  return db.insert('invoices', buildRecord(data));
}
```

### Edge
If the duplicated blocks require different error handling or side effects in each call site, parametrize the helper or keep them separate rather than forcing a leaky abstraction.
