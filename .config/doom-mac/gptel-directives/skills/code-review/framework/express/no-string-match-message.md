---
id: rule-fw-exp-003
title: No String Matching on Response Messages
severity: MEDIUM
tags: string, match, message, error-code, control-flow
---

Never string-match `response.message` for control flow; use `response.code` or HTTP status codes instead.

### Apply
- Any code that inspects an error or response object to determine next steps
- Client-side or server-side code that branches on the content of an API response

### Skip
- Logging or display logic that reads the message for human-readable output only
- Third-party API responses where no error code is provided (document as tech debt)

### Bad
```javascript
const response = await api.createAccount(data);

if (response.message === 'Email already exists') {
  showDuplicateEmailError();
} else if (response.message.includes('invalid')) {
  showValidationError();
} else {
  proceedToNextStep();
}
```

### Good
```javascript
const response = await api.createAccount(data);

if (response.code === 'DUPLICATE_EMAIL') {
  showDuplicateEmailError();
} else if (response.status === 422) {
  showValidationError();
} else if (response.status === 200) {
  proceedToNextStep();
}
```

### Edge
Messages are localized, reworded, or reformatted without notice, breaking string comparisons silently. Error codes and HTTP status codes are part of the API contract and remain stable. If a third-party API only exposes human-readable messages, wrap the check in a named function and document the fragility.
