---
id: rule-u-ada-013
title: Error Message Context
severity: LOW
tags: error, message, context, validation
---

Error text must describe the specific problem in the context of the user's action, not use generic labels.

### Apply
- Form validation messages
- Inline error feedback shown after user input
- Toast or banner errors triggered by a specific user action

### Skip
- System-level errors where specificity is impossible (e.g., network failures)

### Bad
```html
<!-- Vague — user doesn't know what's wrong or how to fix it -->
<span class="error" role="alert">Invalid input</span>

<!-- Generic — which field? what format? -->
<span class="error" role="alert">Please correct the errors above</span>
```

### Good
```html
<!-- Specific field, specific requirement -->
<span class="error" role="alert">Phone number must be 10 digits</span>

<!-- Tied to the exact action the user just attempted -->
<span class="error" role="alert">Email address must include an @ symbol</span>
```

### Edge
"Invalid input" is always bad. Good error messages name the field, state the constraint, and (when possible) suggest the fix. For screen reader users who navigate out of linear order, the message must be self-contained without relying on visual proximity to the field.
