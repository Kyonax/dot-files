---
id: rule-u-sc-007
title: Separate Error Domains
severity: HIGH
tags: error, async, try-catch, domain, await
---

When two sequential awaits serve different purposes, wrap each in its own try/catch so a data-fetch error cannot surface as an auth error or vice versa.

### Apply
- Sequential async operations that belong to different failure domains (authentication, data retrieval, payment, notification, etc.)

### Skip
- Multiple awaits that are part of a single logical transaction where any failure should abort the whole sequence with the same error handling

### Bad
```javascript
async function loadDashboard(token) {
  try {
    const session = await verifyToken(token);
    const profile = await fetchProfile(session.userId);
    const feed = await fetchFeed(session.userId);
    return { profile, feed };
  } catch (err) {
    // A fetchFeed network error is now indistinguishable from an auth failure
    throw new Error('Authentication failed');
  }
}
```

### Good
```javascript
async function loadDashboard(token) {
  let session;
  try {
    session = await verifyToken(token);
  } catch (err) {
    throw new Error('Authentication failed');
  }

  let profile;
  try {
    profile = await fetchProfile(session.userId);
  } catch (err) {
    throw new Error('Failed to load profile');
  }

  let feed;
  try {
    feed = await fetchFeed(session.userId);
  } catch (err) {
    throw new Error('Failed to load feed');
  }

  return { profile, feed };
}
```

### Edge
If multiple awaits share the same error domain and the same recovery strategy, a single try/catch is fine. The rule targets mixed domains where different errors require different messages or recovery paths.
