---
id: rule-u-sc-004
title: No Inline Event Logic
severity: MEDIUM
tags: inline, event, click, handler
---

Event handlers in templates must call a single named method; do not inline logic, assignments, or multi-step expressions in the template.

### Apply
- Click handlers, input handlers, submit handlers, and any DOM event binding in a template
- Applies to both Vue templates and JSX

### Skip
- Simple `$emit` calls with no additional logic (e.g., `@click="$emit('close')"`)

### Bad
```javascript
// In a Vue template
<button @click="loading = true; items = items.filter(i => i.id !== id); submitForm()">
  Save
</button>

// In JSX
<button onClick={() => { setCount(count + 1); logEvent('increment'); }}>
  Add
</button>
```

### Good
```javascript
// In a Vue template
<button @click="handleSave">
  Save
</button>

// In the component
methods: {
  handleSave() {
    this.loading = true;
    this.items = this.items.filter(i => i.id !== id);
    this.submitForm();
  },
},

// In JSX
<button onClick={handleAdd}>
  Add
</button>

function handleAdd() {
  setCount(count + 1);
  logEvent('increment');
}
```

### Edge
A single method call with one argument is fine inline (e.g., `@click="selectItem(item.id)"`). The rule targets multi-statement logic, not parameterized single calls.
