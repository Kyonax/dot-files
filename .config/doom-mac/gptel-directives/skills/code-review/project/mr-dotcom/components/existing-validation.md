---
id: rule-pj-mrd-057
title: Use Existing Validation Systems
severity: MEDIUM
tags: validation, system, parallel, vuelidate, existing
---

Use the existing validation system (Vuelidate) for form validation; do not introduce parallel validation logic.

### Apply
- Any form or input that requires validation

### Skip
- Non-form validation (e.g., business logic checks in Vuex actions)

### Bad
```javascript
data() {
  return {
    emailError: '',
    phoneError: '',
  };
},
methods: {
  validateEmail() {
    if (!this.email.includes('@')) {
      this.emailError = 'Invalid email';
    }
  },
  validatePhone() {
    if (this.phone.length < 10) {
      this.phoneError = 'Phone too short';
    }
  },
},
```

### Good
```javascript
import { useVuelidate } from '@vuelidate/core';
import { required, email } from '@vuelidate/validators';

validations() {
  return {
    email: { required, email },
    phone: { required, minLength: minLength(10) },
  };
},
setup() {
  return { v$: useVuelidate() };
},
```

### Edge
For complex async validation (e.g., checking promo code validity against the API), Vuelidate's async validators should be used. If a validation is truly one-off and not tied to a form field, a simple method check is acceptable.
