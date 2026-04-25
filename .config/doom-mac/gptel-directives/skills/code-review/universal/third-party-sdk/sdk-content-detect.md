---
id: rule-u-sdk-002
title: Poll for SDK DOM With Timeout
severity: MEDIUM
tags: sdk, poll, timeout, content, hide
---

Poll for SDK-injected DOM elements with a timeout and hide the containing section on failure instead of leaving an empty gap.

### Apply
- Any section whose visible content depends entirely on a third-party SDK rendering into the DOM

### Skip
- SDK integrations where the host element has meaningful fallback content that renders independently of the SDK

### Bad
```js
// No detection — section stays visible but empty if SDK fails
mounted() {
  ThirdPartySDK.render(this.$refs.container);
}
```

### Good
```js
mounted() {
  ThirdPartySDK.render(this.$refs.container);
  this.pollForContent();
},
methods: {
  pollForContent() {
    const maxAttempts = 10;
    const interval = 500;
    let attempts = 0;
    const timer = setInterval(() => {
      attempts++;
      const content = this.$refs.container?.querySelector('.sdk-content');
      if (content) {
        this.sdkLoaded = true;
        clearInterval(timer);
      } else if (attempts >= maxAttempts) {
        this.sdkLoaded = false;
        clearInterval(timer);
      }
    }, interval);
  },
}
```

### Edge
On localhost or staging environments, SDKs that require a production domain will consistently time out. The polling + hide pattern ensures the section degrades gracefully to invisible rather than throwing errors or leaving a blank placeholder visible.
