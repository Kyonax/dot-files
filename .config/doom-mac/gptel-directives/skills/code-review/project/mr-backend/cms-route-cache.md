---
id: rule-pj-mrb-003
title: Parameterized Route Cache and Redis Invalidation
severity: MEDIUM
tags: cms, route, cache, redis, PARAM_ROUTES_INVALID
---

CMS parameterized routes are cached in memory on each server instance and refreshed only via the Redis `PARAM_ROUTES_INVALID` broadcast channel.

### Apply
- Adding or modifying CMS pages with parameterized URLs in Tophat
- Debugging stale route resolution or 404s that resolve after server restart
- Changing route configuration that affects how parameterized routes are matched

### Skip
- Static CMS pages without URL parameters (these are not cached in the parameterized route map)
- Client-side route changes that do not hit the Express CMS middleware

### Bad
```javascript
// Assuming route changes in Tophat take effect immediately
// Deploying a new parameterized CMS page and expecting it to work
// without triggering cache invalidation
```

### Good
```javascript
// After updating parameterized routes in Tophat, invalidate the cache:
// Tophat publishes to Redis channel PARAM_ROUTES_INVALID
// All server instances receive the broadcast and refresh their in-memory cache

// If debugging stale routes, check:
// 1. Redis pub/sub is connected (MRRedis subscription active)
// 2. The PARAM_ROUTES_INVALID handler clears and rebuilds the route map
// 3. Server logs confirm cache refresh after Tophat publish
```

### Edge
Each server instance maintains its own in-memory copy of the parameterized route map. If a server misses the Redis broadcast (e.g., Redis connection blip), that instance serves stale routes until the next restart or successful broadcast. During debugging, check individual server instances — not all may have the same route map. A full deploy or rolling restart guarantees all instances get the latest routes.
