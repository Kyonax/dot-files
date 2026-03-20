---
title: Standard Operating Procedure for Creating a Skill from Scratch
impact: HIGH
impactDescription: Defines the mandatory 5-phase process for building a new skill. Following the wrong order (e.g., writing SKILL.md before rules) produces inaccurate routing tables and keyword mismatches.
tags: skill, creation, sop, process, phases, ground-truth, categorize, mining, source-material, workflow, new-skill, from-scratch, blueprint, domain, knowledge
---

This rule defines the mandatory 5-phase process for building a new skill from scratch. Following the wrong order — especially writing SKILL.md before the rules — produces routing tables that don't match actual content, keyword mismatches that break triggering, and Quick Reference descriptions written from plans rather than reality.

## The 5 Phases (Mandatory Order)

Skills must be built in this specific order. The order is not arbitrary — each phase depends on the output of the previous one.

```
Phase 1: Ground Truth Mining
    ↓ produces: raw patterns, conventions, domain samples
Phase 2: Categorization
    ↓ produces: list of atomic rule categories
Phase 3: Write Rules (Worker First)
    ↓ produces: complete rules/*.md files
Phase 4: Write SKILL.md (Router Last)
    ↓ produces: accurate routing table and descriptions
Phase 5: Test & Iterate
    ↓ produces: validated, refined skill
```

**Why this order matters:** If you write SKILL.md first (Phase 4), its routing table and Quick Reference descriptions will be based on *planned* content, not *actual* content. After writing the rules (Phase 3), you often discover that categories shifted, topics merged, or scope changed. Writing the router last ensures it accurately reflects what was actually written.

---

## Phase 1: Ground Truth Mining

**Goal:** Gather all raw source material and real-world patterns for the skill's domain.

**Actions:**
1. **Explore the source material** — Read representative artifacts (code files, documents, templates, workflows, research papers, policies, specifications)
2. **Note conventions** — Naming patterns, organizational structure, formatting standards, terminology, stylistic choices
3. **Identify tribal knowledge** — Patterns practitioners follow but aren't documented anywhere
4. **Collect external sources** — Documentation, style guides, spec files, wiki pages, industry standards, regulatory references

**For source material transformation** (when the source has a different format):
- **Abstract** the domain knowledge from the source structure
- **Never replicate** the source format — transform it into the Three-File blueprint
- Example: An SEO skill was created from an external open-source project with a completely different file structure. The source format was disregarded entirely — domain knowledge was extracted, recategorized into atomic rules, and restructured into the standard directory layout.

**Output:** A collection of raw patterns, domain samples, and conventions — unstructured, but comprehensive.

---

## Phase 2: Categorization

**Goal:** Break the raw material into atomic, independent categories. Each category becomes one rule file.

**Actions:**
1. **Group related patterns** — Look for natural clusters (all items in one sub-discipline, all patterns for one workflow, all rules for one concern)
2. **Apply the splitting test** for each proposed category: "Can I imagine a task that needs this category but NOT another category in the same group?"
   - If yes → they are separate rules
   - If no → they can stay together
3. **Name each category** with a descriptive `kebab-case` name that describes its specific scope
4. **Assign impact levels** — CRITICAL, HIGH, MEDIUM, LOW based on the consequence of violation

**Categorization examples (illustrative):**

| Domain             | Categories Created                                                                                      | Splitting Logic                                                                                  |
|--------------------|---------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------|
| SEO                | `technical-seo`, `on-page-seo`, `structured-data`, `mobile-seo`, `international-seo`, `audit-checklist` | Each SEO sub-discipline is independently needed — structured data work doesn't need mobile rules |
| Legal Compliance   | `data-privacy`, `accessibility-requirements`, `terms-of-service`, `regulatory-filings`                  | Each compliance area is independently needed — privacy work doesn't need accessibility rules     |
| Research Methods   | `literature-review`, `data-collection`, `analysis-frameworks`, `citation-standards`                     | Each research phase is independently needed — data collection doesn't need citation rules        |
| Document Generator | `index-management`, `data-parsing`, `format-reference`, `templates`                                     | Each generation concern is independently needed — template work doesn't need index logic         |

**Output:** A list of rule file names with scope descriptions and impact levels.

---

## Phase 3: Write Rules (Worker First)

**Goal:** Create complete, exhaustive rule files for each category identified in Phase 2.

**Actions (per rule file):**

1. **Write YAML frontmatter** — `title`, `impact`, `impactDescription`, `tags`
2. **Write opening paragraph** — What this rule governs and why it matters
3. **Write exhaustive body** — Tables for enumerable data, correct/incorrect examples, complete references
4. **Verify self-containment** — Can the rule be understood without reading any other rule?
5. **Verify atomicity** — Does the rule cover exactly one separable concern?

**Key principles during this phase:**
- **"Explicit over Implicit"** — List every item, don't summarize patterns
- **Tables over prose** for enumerable data (items, options, criteria, configurations)
- **At least 2 correct/incorrect example pairs** per major concept
- **Include inline context** for any term or concept defined in another rule

**Output:** Complete `rules/*.md` files with frontmatter and exhaustive content.

---

## Phase 4: Write SKILL.md (Router Last)

**Goal:** Create the routing manifest that accurately maps tasks to the rules written in Phase 3.

**Actions:**

1. **Write YAML frontmatter** — `name` (matches directory), `description` (keyword-dense, <1024 chars)
2. **Write one-line summary** — What the skill provides
3. **Write "When to Apply"** — Bullet list of specific scenarios
4. **Write routing table** — Map task contexts to specific rule files
   - Every rule file must appear in at least one row
   - A task may require multiple rules
   - Be specific about contexts — avoid catch-all rows
5. **Write Quick Reference** — One keyword-rich description per rule
   - Must summarize the FULL scope of each rule (not just the title)
   - These descriptions are what the GPTel Analyzer reads for routing decisions
6. **Verify size** — Must be under 500 lines

**Critical check:** Re-read each rule file's actual content, then verify the Quick Reference description accurately reflects it. Descriptions written from memory of Phase 2 plans (before the rules were written) are often inaccurate.

**Output:** Complete `SKILL.md` file with frontmatter, routing table, and Quick Reference.

---

## Phase 5: Test & Iterate

**Goal:** Validate the skill works correctly in real usage.

**Actions:**

1. **Manual invocation** — Use `/skill-name` in Claude Code to verify it loads
2. **Prompt testing** — Try realistic prompts and check:
   - Does the skill activate? (description quality)
   - Do the right rules load? (routing table accuracy)
   - Does the Worker produce correct output? (rule content quality)
3. **Edge case testing** — Try prompts that should load only one specific rule — verify others don't load
4. **Refinement:**
   - If skill doesn't activate → refine `description` with more keywords
   - If wrong rules load → refine routing table specificity
   - If output is wrong → refine rule content (more examples, more explicit data)
   - If too much loads → split rules further or tighten routing conditions

**Output:** A validated, production-ready skill.

---

## Correct vs. Incorrect Process Examples

### Example 1: Phase Order

**Incorrect:** Writing SKILL.md first, then rules.
```
Phase 1: Write SKILL.md with planned routing table
Phase 2: Write rules based on what SKILL.md says
```

**Correct:** Writing rules first, SKILL.md last.
```
Phase 3: Write all rule files from ground truth
Phase 4: Write SKILL.md routing table from actual rule content
```

**Why:** Plans change during rule writing — categories merge, topics shift, scope narrows. A routing table written from plans (not reality) will have keyword mismatches and inaccurate Quick Reference descriptions.

### Example 2: Ground Truth Mining

**Incorrect:** Writing rules from assumptions.
```markdown
## Phase 1 output: "I think SEO skills need technical SEO, content SEO, and link building."
## Phase 3: Writes rules based on assumption — misses structured data, mobile SEO, international SEO.
```

**Correct:** Mining actual source material first.
```markdown
## Phase 1 output: Read Google's SEO docs, reviewed 3 audit tools, analyzed team's SEO checklist.
## Phase 2: Identified 6 atomic categories from real patterns — including structured-data and mobile-seo.
```

**Why:** Assumptions produce incomplete skills. Ground truth mining surfaces patterns, edge cases, and categories that assumptions miss.

---

## Quick Start Checklist

For a new skill from zero:

- [ ] **Phase 1:** Identify and read all source material (domain artifacts, external docs, references)
- [ ] **Phase 2:** List all proposed rule files with names, scopes, and impact levels
- [ ] **Phase 2:** Apply the splitting test to every proposed rule
- [ ] **Phase 3:** Create `rules/` directory
- [ ] **Phase 3:** Write each rule file with YAML frontmatter and exhaustive content
- [ ] **Phase 3:** Verify each rule is self-contained and atomic
- [ ] **Phase 4:** Write SKILL.md with frontmatter, routing table, Quick Reference
- [ ] **Phase 4:** Verify Quick Reference descriptions match actual rule content
- [ ] **Phase 4:** Verify SKILL.md is under 500 lines
- [ ] **Phase 5:** Test with realistic prompts in Claude Code
- [ ] **Phase 5:** Verify selective loading works (only relevant rules load)
