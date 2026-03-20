---
title: Refining, Splitting, and Evolving Existing Skills
impact: HIGH
impactDescription: Skills are living documents that must evolve as patterns emerge and domains grow. Incorrect refactoring breaks routing accuracy, creates orphaned rules, or introduces cross-rule dependencies.
tags: skill, refine, update, split, evolve, refactor, monolithic, version, promote, session, guidelines, merge, deprecate, restructure
---

This rule governs how to update, split, and evolve existing skills as domains grow and patterns emerge. Incorrect refactoring breaks routing accuracy (Quick Reference no longer matches rules), creates orphaned rules (no routing path), or introduces cross-rule dependencies that violate the self-containment principle.

## When to Refine a Skill

| Trigger                                                   | Action                                                        |
|-----------------------------------------------------------|---------------------------------------------------------------|
| A rule file exceeds ~500 lines                            | Split into multiple atomic rules                              |
| A SKILL.md file is monolithic (all content, no rules/)    | Extract knowledge into rule files, convert SKILL.md to router |
| Session guidelines have stabilized                        | Promote them into the relevant skill's rules                  |
| A rule covers two independently-needed concerns           | Split into two rules                                          |
| Quick Reference descriptions no longer match rule content | Rewrite descriptions from actual rule content                 |
| A rule is never loaded (no routing path)                  | Either add a routing path or remove the rule                  |
| Description field doesn't trigger for known use cases     | Add more keywords and trigger conditions                      |

---

## Pattern 1: Splitting a Monolithic File

**When:** A single file (often SKILL.md itself) contains all domain knowledge with no `rules/` directory.

**Process:**

1. **Identify natural boundaries** — Look for major sections, headings, or topic shifts
2. **Apply the splitting test** to each candidate boundary: "Can I imagine needing section A without section B?"
3. **Create rule files** for each independent concern
4. **Move content** from the monolithic file into the appropriate rule files
5. **Add YAML frontmatter** to each new rule file
6. **Convert the source file** into a SKILL.md router (if it was SKILL.md) or delete it (if it was an intermediate file)
7. **Write the routing table** mapping tasks to the new rule files
8. **Update Quick Reference** to reflect the new rule set

**Example — splitting a 1000+ line monolithic SKILL.md:**

| Source Section (in monolithic file)                                        | Target Rule File            | Rationale                                                    |
|----------------------------------------------------------------------------|-----------------------------|--------------------------------------------------------------|
| Index/database management (workflows, update flows, lifecycle rules)       | `rules/index-management.md` | Index operations are independent of content generation       |
| Input format parsing (format variants, field mapping, content translation) | `rules/data-parsing.md`     | Parsing logic is independent of output formatting            |
| Output format reference (syntax rules, escaping, special characters)       | `rules/format-reference.md` | Output syntax is independent of input parsing or index logic |
| Templates (standard, variant, base structure, formatting)                  | `rules/templates.md`        | Template structure is independent of parsing or indexing     |

---

## Pattern 2: Adding Rules to an Existing Skill

**When:** A skill has identified gaps — topics within its domain that lack rule files.

**Process:**

1. **Follow Phases 1-3 of the Creation SOP** for the new rule only
2. **Verify the new rule is self-contained** — no dependencies on existing rules without inline context
3. **Add a routing row** in SKILL.md's routing table for the new rule
4. **Add a Quick Reference entry** with a keyword-rich description
5. **Bump the version** in SKILL.md frontmatter metadata

**Gap identification example:**

| Existing Rules                              | Identified Gaps                                                     | Priority           |
|---------------------------------------------|---------------------------------------------------------------------|--------------------|
| technical-seo, on-page-seo, structured-data | mobile-seo, international-seo, audit-checklist, performance-metrics | CRITICAL to MEDIUM |

Each gap becomes a new Phase 1-3 cycle. The SKILL.md routing table grows but its structure stays the same.

---

## Pattern 3: Promoting Session Guidelines into Skills

**When:** A session context block (Section 1: Global Guidelines) contains patterns that have proven stable across multiple conversations and should become permanent skill rules.

**Process:**

1. **Identify stable guidelines** — Patterns that haven't changed across 2+ sessions
2. **Determine which skill owns the domain** — Where does this guideline belong?
3. **Check for existing rule overlap** — Does an existing rule already cover part of this?
4. **Either:**
   - **Extend an existing rule** — Add the guideline as a new section in the appropriate rule file
   - **Create a new rule** — If the guideline represents a separable concern not covered by any existing rule
5. **Remove the guideline from the session file** — It now lives in the skill
6. **Update SKILL.md** — Routing table and Quick Reference if a new rule was created

**The session file is a staging area.** Guidelines start there when first discovered, prove their stability through repeated use, then graduate into permanent skill rules.

---

## Pattern 4: Restructuring a Skill's Rule Organization

**When:** The initial categorization no longer serves the skill well — rules are too broad, too narrow, or overlap.

**Process:**

1. **Map current routing accuracy** — For each rule, list which tasks load it and whether the load is justified
2. **Identify problems:**
   - Rules loaded for tasks that don't need them → rule is too broad, split it
   - Multiple rules always loaded together → rules are too narrow, merge them
   - Overlapping content between rules → extract shared content into one rule, reference inline
3. **Plan the new structure** before making changes
4. **Execute the restructure:**
   - Create new rule files
   - Move content (do not copy — avoid duplication)
   - Update all YAML frontmatter
   - Rewrite SKILL.md routing table completely
   - Rewrite Quick Reference completely
5. **Test** — Verify the new routing loads the right rules for known task scenarios

---

## Pattern 5: Version Bumping

**When to bump:**

| Change Type                              | Version Bump  | Example                              |
|------------------------------------------|---------------|--------------------------------------|
| New rule file added                      | Minor (x.Y.0) | v2.3.0 → v2.4.0                      |
| Existing rule content updated            | Patch (x.y.Z) | v2.3.0 → v2.3.1                      |
| Rules restructured or split              | Minor (x.Y.0) | v2.3.0 → v3.0.0 if major restructure |
| SKILL.md routing table changed           | Patch (x.y.Z) | v2.3.0 → v2.3.1                      |
| Breaking change (rules renamed, removed) | Major (X.0.0) | v2.3.0 → v3.0.0                      |

Update the `version` field in SKILL.md YAML frontmatter metadata.

---

## Correct vs. Incorrect Refinement Examples

### Example 1: Splitting a Rule

**Incorrect:** Keeping two independent concerns in one file.
```
rules/compliance.md (covers GDPR, accessibility, and terms of service)
→ Loaded for GDPR task, but 60% of content (accessibility + ToS) is irrelevant.
```

**Correct:** Splitting into atomic rules.
```
rules/data-privacy.md (GDPR + CCPA only)
rules/accessibility-requirements.md (WCAG + ADA only)
rules/terms-of-service.md (ToS + legal copy only)
→ GDPR task loads only data-privacy.md. Zero wasted tokens.
```

**Why:** The splitting test asks: "Can I imagine needing GDPR rules without accessibility rules?" Yes — so they must be separate files.

### Example 2: Updating Quick Reference After Rule Changes

**Incorrect:** Changing rule content without updating SKILL.md.
```markdown
# rule-writing.md was updated to add Containerization Rule (Section 5)
# SKILL.md Quick Reference still says: "mandatory YAML frontmatter, exhaustive content, splitting strategy"
# → Analyzer has no keywords for "containerization" — rule never loads for containerization tasks
```

**Correct:** Re-reading the rule and updating the Quick Reference.
```markdown
# After updating rule-writing.md:
# SKILL.md Quick Reference updated to: "...atomic splitting strategy, selective loading principles,
#   containerization rule (no external references), correct-vs-incorrect examples..."
# → Analyzer now has "containerization" keyword — loads rule for relevant tasks
```

**Why:** The Quick Reference is what the Analyzer reads. Stale descriptions cause under-loading (missing rules) or mis-routing.

---

## Refinement Checklist

Before finalizing any skill modification:

- [ ] All rule files have valid YAML frontmatter (4 required fields)
- [ ] No rule covers two independently-needed concerns (splitting test passes)
- [ ] Every rule is self-contained (no unexplained cross-references)
- [ ] SKILL.md routing table includes every rule file
- [ ] Quick Reference descriptions match actual rule content (re-read rules to verify)
- [ ] SKILL.md is under 500 lines
- [ ] Version has been bumped appropriately
- [ ] No orphaned rules (rules with no routing path)
- [ ] No orphaned routing rows (rows pointing to non-existent rules)
