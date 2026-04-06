---
title: Writing Atomic, Self-Contained Rule Files with Selective Loading
impact: CRITICAL
impactDescription: Rule files are the Worker AI's only source of domain knowledge. Poorly written rules cause wrong output; poorly split rules waste tokens by loading irrelevant content. This rule governs both content quality and structural precision.
tags: rule, writing, atomic, self-contained, selective-loading, frontmatter, exhaustive, explicit, implicit, tables, examples, splitting, separation-of-concerns, token-efficiency
---

This rule governs the creation and quality of rule files (`rules/*.md`) — the Worker AI's only source of domain knowledge. Poorly written rules produce wrong output; poorly split rules waste tokens by loading irrelevant content for half of tasks. Every rule file must be atomic (one concern), self-contained (no cross-rule dependencies), exhaustive (every item listed explicitly), and containerized (no external references).

## 1. The Selective Loading Imperative

**Rule files are optional resources, not auto-loaded content.** They are read by the Worker AI only when SKILL.md's routing table directs it to. This means:

- **Every rule file load has a token cost.** Loading a 300-line rule for a task that doesn't need it wastes tokens and dilutes the Worker's focus.
- **Every rule file must justify its own loading.** If someone asks "why was this rule loaded for this task?", there must be a clear answer.
- **If a rule file covers two separable concerns, it MUST be split.** The test: "Can I imagine a task that needs concern A but not concern B?" If yes, they are separate rules.

### The Splitting Decision Matrix

| Situation                                             | Action             | Example                                                                                     |
|-------------------------------------------------------|--------------------|---------------------------------------------------------------------------------------------|
| File covers one focused topic                         | Keep as-is         | `technical-seo.md` — only crawlability, indexing, and site structure                        |
| File covers two topics always needed together         | Keep as-is         | A rule covering both "when to apply" and "how to implement" for one concern                 |
| File covers two topics sometimes needed independently | **Split**          | `writing-standards.md` → `tone-guidelines.md` + `formatting-rules.md` + `citation-style.md` |
| File exceeds ~500 lines                               | Evaluate for split | Look for natural section boundaries that represent independent concerns                     |
| File is a monolithic SKILL.md with no rules/          | **Split urgently** | A 1000+ line SKILL.md with all knowledge inline → extract into 4 focused rule files         |

### Naming Split Files

When splitting, name each file after the specific concern it covers:

| Before (monolithic)       | After (split)                                                                                      |
|---------------------------|----------------------------------------------------------------------------------------------------|
| `seo-best-practices.md`   | `technical-seo.md`, `on-page-seo.md`, `structured-data.md`, `mobile-seo.md`                        |
| `legal-compliance.md`     | `data-privacy.md`, `accessibility-requirements.md`, `terms-of-service.md`, `regulatory-filings.md` |
| `research-methodology.md` | `literature-review.md`, `data-collection.md`, `analysis-frameworks.md`, `citation-standards.md`    |

## 2. Mandatory YAML Frontmatter

Every rule file must start with this exact structure:

```yaml
---
title: Clear, Actionable Title for This Rule
impact: CRITICAL | HIGH | MEDIUM | LOW
impactDescription: One concise sentence on why this rule matters and what it prevents.
tags: comma, separated, keywords, for, semantic, matching
---
```

### Frontmatter Field Rules

**`title`:**
- Must describe the rule's scope, not just its topic
- Good: `"Data Privacy Compliance — GDPR & CCPA Requirements"` or `"Spacing Utility Classes — Margin & Padding System"`
- Bad: `"Privacy"` or `"Spacing"`

**`impact`:**
- `CRITICAL` = Wrong output without this rule. Worker generates incorrect or harmful results.
- `HIGH` = Suboptimal output. Results are functional but violate conventions or cause subtle issues.
- `MEDIUM` = Inconsistent output. Formatting, naming, or process deviations.
- `LOW` = Quality improvement. Nice-to-have guidance.

**`impactDescription`:**
- Must explain what goes wrong WITHOUT this rule, not just what the rule covers
- Good: `"Prevents non-compliant data handling and ensures all user consent flows meet GDPR/CCPA requirements."` or `"Eliminates magic numbers and ensures consistent spacing across all components."`
- Bad: `"Covers privacy."` or `"Covers spacing classes."`

**`tags`:**
- Include all keywords that a routing agent might search for
- Include synonyms and related terms
- These are used by routing analyzers for semantic matching
- Err on the side of more tags, not fewer

## 3. Content Structure

### Opening: What and Why

Start with 1-2 paragraphs explaining:
1. What this rule governs (scope)
2. Why it matters (consequence of violation)

No heading needed for this opening — it follows directly after the frontmatter.

### Body: Exhaustive Reference

The body must be an **exhaustive, explicit reference** for the Worker AI. This is where "Explicit over Implicit" lives.

**Use tables for enumerable data:**

```markdown
## Example A: Code Domain — Available Spacing Values

| Value | em Equivalent | Usage |
|---|---|---|
| 0 | 0 | Reset spacing |
| 50 | 0.5em | Medium spacing |
| 100 | 1em | Standard spacing |
| 200 | 2em | Extra large spacing |

## Example B: Compliance Domain — Required Consent Types

| Consent Type | When Required | Regulatory Basis |
|---|---|---|
| Explicit opt-in | Before collecting personal data | GDPR Art. 6(1)(a) |
| Cookie consent | Before setting non-essential cookies | ePrivacy Directive |
| Marketing consent | Before sending promotional emails | CAN-SPAM / GDPR |
```

**IMPORTANT — Keep table cells short. Extract verbose content to detail blocks:**

Table cells must be scannable by both humans and AI. If a cell exceeds ~15 words, put a short summary in the cell and reference a named detail block below the table.

```markdown
## Pattern: Short Cell + Detail Reference

| Rule | Description |
|---|---|
| 12 | Module-level constants for static data. [Details](#rule-12) |
| 13 | No inline event logic — `@click` calls a single method |

#### Rule 12
Full explanation with code examples, correct/incorrect pairs, etc.
```

**Why:** Long table cells break column alignment, hurt readability, and cause AI agents to misparse boundaries between cells. Short cells with `[Details](#anchor)` links keep tables scannable while preserving depth in the referenced sections.

**Use correct/incorrect example pairs:**

```markdown
## Example A: Code Domain

**Incorrect:** Hard-coded values in styles.
` ` `css
.my-component { margin-top: 16px; }
` ` `

**Correct:** Design system tokens.
` ` `css
.my-component { margin-top: var(--spacing-100); }
` ` `

**Why:** Design tokens enforce the spacing scale and prevent magic numbers.

## Example B: Writing Domain

**Incorrect:** Vague, passive construction.
> The data was processed by the system.

**Correct:** Clear, active construction with specific attribution.
> The ETL pipeline processes 50,000 records per batch run.

**Why:** Active voice with specifics improves clarity and enables accountability.
```

### Self-Containment Rule

**Each rule must be understandable without reading any other rule file.** If a rule references concepts from another rule:

| Situation                                      | What to do                                                  |
|------------------------------------------------|-------------------------------------------------------------|
| Brief reference to a concept covered elsewhere | Include a 1-2 sentence inline summary of the concept        |
| Heavy dependency on another rule's content     | The rules are not properly separated — reconsider the split |
| Shared terminology                             | Define terms inline in each rule that uses them             |

**Anti-pattern:** "For details, see `rules/other-rule.md`" without any inline context. If the Worker loaded this rule but not the referenced one, it would have a knowledge gap.

## 4. Content Completeness Checklist

Before finalizing a rule file, verify:

- [ ] **YAML frontmatter** is present with all 4 required fields
- [ ] **`title`** describes scope, not just topic
- [ ] **`impactDescription`** explains what goes wrong without this rule
- [ ] **`tags`** include all relevant keywords and synonyms
- [ ] **Opening paragraph** explains scope and consequence
- [ ] **Enumerable data** is in tables (not prose)
- [ ] **At least 2 correct/incorrect example pairs** are included
- [ ] **Self-contained** — no unexplained references to other rules
- [ ] **One concern per file** — the splitting test passes
- [ ] **Exhaustive** — every item, option, criterion, or pattern is listed (not summarized)

## 5. Containerization Rule

**All data inside a skill must be self-contained — no external references.** A skill must never reference other skills by name, specific projects, user-specific paths, or external systems. Everything the Worker needs must exist inside the skill's own directory.

| Violation                                                                        | Why it breaks                                   | Fix                                                                                    |
|----------------------------------------------------------------------------------|-------------------------------------------------|----------------------------------------------------------------------------------------|
| Referencing another skill by name (e.g., "see the `my-other-skill` for details") | Target skill may not exist on the user's system | Inline the needed knowledge, or use a generic illustrative example                     |
| Referencing a specific project or repository                                     | Skill becomes non-portable                      | Use generic domain examples (e.g., "a CSS utility skill" instead of naming a real one) |
| Using user-specific paths (e.g., `/Users/john/...`)                              | Breaks on any other system                      | Use placeholder paths (e.g., `/path/to/your/skills/`) or relative references           |
| Referencing external tools or scripts not bundled with the skill                 | Worker cannot access them                       | Move helpers into `scripts/`, reference data into `references/` or `assets/`           |
| Using jargon or terminology defined only outside the skill                       | Worker has a knowledge gap                      | Define all terms inline within the skill                                               |

**The test:** If the skill directory were cloned to a fresh system with zero other skills installed, could the AI still produce correct output from any rule file? If not, the skill has an external dependency that must be internalized.

## 6. Common Anti-Patterns

| Anti-Pattern               | Problem                                                 | Fix                                                  |
|----------------------------|---------------------------------------------------------|------------------------------------------------------|
| Formula instead of list    | Worker cannot derive all values from a pattern          | List every value explicitly in a table               |
| "And similar patterns"     | Worker guesses wrong                                    | Show every pattern explicitly                        |
| Two topics in one file     | Loads irrelevant content for half of tasks              | Split into two files                                 |
| Vague `impactDescription`  | Analyzer cannot judge relevance                         | Explain the specific failure mode                    |
| Missing examples           | Worker has no reference for correct usage               | Add 2-5 correct/incorrect pairs                      |
| Cross-rule dependency      | Rule not self-contained; breaks if loaded alone         | Inline the needed context                            |
| Monolithic 1000+ line file | Cannot selectively load subsections                     | Split into atomic rules along natural boundaries     |
| External references        | Skill breaks on systems without the referenced resource | Internalize all knowledge inside the skill directory |
