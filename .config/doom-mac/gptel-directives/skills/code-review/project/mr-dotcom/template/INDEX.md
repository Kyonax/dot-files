# project/mr-dotcom/template/ — MR Template Rules

6 rules. Loaded when project detected as mr-dotcom.

| ID | File | Summary | Severity |
|---|---|---|---|
| rule-pj-mrd-001 | pug-only.md | All templates use Pug (lang="pug"), no HTML templates | MEDIUM |
| rule-pj-mrd-002 | kebab-case-tags.md | Vue components in kebab-case in Pug: mr-btn not MrBtn, mr-icon not MrIcon | MEDIUM |
| rule-pj-mrd-003 | heading-inline-text.md | Heading text inline in Pug: h2 Title, not h2 followed by pipe text | MEDIUM |
| rule-pj-mrd-004 | curly-braces-if.md | Always use curly braces for if statements, even single-line returns | MEDIUM |
| rule-pj-mrd-005 | single-h1-page.md | Single h1 per page; section components use h2 or lower | MEDIUM |
| rule-pj-mrd-006 | omit-redundant-div.md | Omit redundant div in Pug; .my-class auto-creates div | MEDIUM |

**Worker instructions:** Review Pug templates against MR template conventions. Report YAML. Return `NO VIOLATIONS` if clean.
