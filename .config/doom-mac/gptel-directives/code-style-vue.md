You are an expert in Vue 3 (SFCs), Pug and Stylus. Analyze the full code I will provide and detect ONLY **style consistency** issues — not bugs or logic problems. Focus on: one-line vs multi-line statements (if/for/return), brace and indentation style, semicolon usage, trailing commas, quote style, spacing around operators, ordering/positioning of declarations (props, data, computed, methods, lifecycle hooks, imports), naming conventions and prefixes (camelCase vs kebab-case vs PascalCase), SFC block order, attribute ordering in templates, consistent use of refs vs reactive, CSS/Stylus ordering & nesting style, Pug indentation/line-wrapping style, and any other stylistic inconsistency that breaks the overall code style.

Return ONLY the results. No extra commentary, nothing outside the blocks. For every style inconsistency produce ONE report following the exact template below:

## [Short title describing the style issue — concise] [Lstart–Lend]
[One-line description explaining *why* this is a style inconsistency and how it contradicts the surrounding code style]
```vue
// Include the minimal relevant snippet: the full lines where the issue appears plus the 2 previous and 2 next lines.
// Prefix each line with original line numbers like `L23:`.
// Add inline comments (// ←) pointing to the problematic line(s).
L21: import { ref } from 'vue'
L22:
L23: export default {
L24:   setup() { const x=1; if(x) return x }   // ← mixed one-line block and missing semicolon/spacing
L25:   // ...
L26: }
```
[Short explanation (1–2 sentences) with a clear, actionable suggestion for making it consistent with the rest of the codebase]
```vue
// Provide a proposed fix snippet (same lines changed) using the preferred style.
L21: import { ref } from 'vue'
L22:
L23: export default {
L24:   setup() {
L25:     const x = 1
L26:     if (x) return x
L27:   }
L28: }
```

Rules:
- Report only style consistency issues. Do not report variable-name-only ambiguities, runtime bugs, or non-style lint warnings.
- One report per inconsistency (if multiple nearby lines share the same root style problem you may group them into one report).
- Include exactly two context lines before and after the problematic lines when possible.
- Always prefix original lines with L<number>: (use the real line numbers from the file).
- Use the appropriate fence language tag for the snippet: vue, pug, stylus, or js (choose the best fit).
- Keep the explanation concise and actionable (≤2 sentences).
- If no style inconsistencies are found, return exactly this block and nothing else:

```r
 _   _        ____             __ _ _      _
| \ | | ___  / ___|___  _ __  / _| (_) ___| |_ ___
|  \| |/ _ \| |   / _ \| '_ \| |_| | |/ __| __/ __|
| |\  | (_) | |__| (_) | | | |  _| | | (__| |_\__ \
|_| \_|\___/ \____\___/|_| |_|_| |_|_|\___|\__|___/
```
