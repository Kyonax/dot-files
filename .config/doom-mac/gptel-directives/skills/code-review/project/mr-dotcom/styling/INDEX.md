# project/mr-dotcom/styling/ — MR Styling Rules

13 rules. Loaded when project detected as mr-dotcom.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-pj-mrd-010 | scoped-stylus.md | All styles use scoped Stylus: `<style scoped lang="stylus">` | CRITICAL |
| rule-pj-mrd-011 | utility-first.md | Utility-first with breakpoint prefixes (.xs-mb-100m, .lg-pt-200m) | CRITICAL |
| rule-pj-mrd-012 | flex-in-stylus.md | Keep flex layout in Stylus, not utility classes | MEDIUM |
| rule-pj-mrd-013 | max-at-tweak.md | .max-at-tweak mandatory on every responsive font class | CRITICAL |
| rule-pj-mrd-014 | design-system-vars.md | Design system variables for colors; no hardcoded hex | MEDIUM |
| rule-pj-mrd-015 | css-alphabetized.md | CSS properties alphabetized within each Stylus block | LOW |
| rule-pj-mrd-016 | no-stylus-spacing.md | No padding/margin in Stylus when utility class exists | MEDIUM |
| rule-pj-mrd-017 | nesting-mirrors.md | Stylus nesting must mirror Pug template hierarchy | MEDIUM |
| rule-pj-mrd-018 | font-in-template.md | Font sizes, font-family, text color in templates only, never in Stylus | CRITICAL |
| rule-pj-mrd-019 | upper-f-secondary.md | .upper mandatory on every .f-secondary heading (Kapra Neue) | MEDIUM |
| rule-pj-mrd-020 | rem-em-units.md | rem/em for spacing; px for borders/shadows only | MEDIUM |
| rule-pj-mrd-021 | styles-inside-sfc.md | All styles inside the SFC; no external .styl/.css files | MEDIUM |
| rule-pj-mrd-022 | comment-3p-overrides.md | Comment :deep() overrides identifying SDK/version | LOW |

**Worker instructions:** Review Stylus styles against MR styling conventions. Report YAML. Return `NO VIOLATIONS` if clean.
