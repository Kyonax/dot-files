---
id: rule-fw-vca-001
title: Script Setup Ordering Convention
severity: MEDIUM
tags: script-setup, composition-api, ref, computed, order
---

In `<script setup>`, follow this ordering convention: imports, store/composables, props/emits, refs, computed, methods/functions, lifecycle hooks.

### Apply
- All components using `<script setup>`
- When adding new declarations to an existing `<script setup>` block

### Skip
- Components using Options API (see `rule-fw-vue-003` for Options API field order)
- Extremely small components with only 2-3 declarations where order is trivially clear

### Bad
```javascript
// <script setup>
const count = ref(0);
import { ref, computed, onMounted } from 'vue';
onMounted(() => fetchData());
const props = defineProps({ id: String });
import { useAuthStore } from '@/stores/auth';
const doubled = computed(() => count.value * 2);
const auth = useAuthStore();
function fetchData() { /* ... */ }
```

### Good
```javascript
// <script setup>
import { ref, computed, onMounted } from 'vue';
import { useAuthStore } from '@/stores/auth';

const auth = useAuthStore();

const props = defineProps({ id: String });
const emit = defineEmits(['update']);

const count = ref(0);

const doubled = computed(() => count.value * 2);

function fetchData() { /* ... */ }

onMounted(() => fetchData());
```

### Edge
The Vue specification does not enforce any particular order inside `<script setup>`. This is a readability convention. The key constraint is that `defineProps` and `defineEmits` must be top-level (Vue compiler requirement), so placing them near the top is both conventional and practical.
