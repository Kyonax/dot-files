---
title: CV Summary Recipe Template — Slot System and Canonical Worked Example
impact: CRITICAL
impactDescription: Defines the exact slot-by-slot template every CV summary must follow. Without this template, summary edits drift into multi-sentence prose forms that fail the single-sentence mandate, break ATS keyword density targets, or lose the participle bridge that connects identity to delivery.
tags: cv, summary, recipe, template, slots, canonical, structure, single-sentence, professional-summary, linkedin, bio, hero-copy, about-page, worked-example
---

This rule defines the canonical 10-slot template that every CV/LinkedIn/About-page summary paragraph must follow. The template was forged through 12 iterations of drafting with a user reviewer. Future drafts MUST fill the slots — not derive a new structure. Re-deriving wastes hours; the slots already encode every constraint discovered in those iterations.

## The Slot Template

The entire summary is **one continuous sentence** built by chaining these exact slots without any periods or paragraph breaks until the final terminal period:

```
[Identity & Experience]
+ [Core Output]
+ "using"
+ [Tech Stack]
+ ","
+ [Bridging "-ing" Verb]
+ [Specialized Domains]
+ "in"
+ [Environment/Methodology]
+ ","
+ [Soft Skills Tail]
+ "."
```

| Slot | Purpose | What to fill |
|---|---|---|
| **[Identity & Experience]** | Who you are + how long | Title (Senior/Lead/Staff Engineer) + years (`8+ years`, `10+ years`) |
| **[Core Output]** | What you build | Domain-specific platforms/products (`scalable, high-performance e-commerce platforms, SaaS products, and consumer-facing web applications`) |
| `using` | Literal connector | Same word always |
| **[Tech Stack]** | Named technologies | Comma-separated, each in italics (`*Vue 3*, *React*, *Next.js*, *TypeScript*, and *Node.js*`) |
| `,` | Literal comma | Bridges to the second half |
| **[Bridging "-ing" Verb]** | Connects identity-half to domain-half | Acceptable: `integrating`, `combining`, `architecting`, `bridging` |
| **[Specialized Domains]** | What disciplines you bring | Domain practices (`frontend architecture, performance and conversion optimization, accessibility, SEO, and AI-assisted development`) |
| `in` | Literal connector | Same word always |
| **[Environment/Methodology]** | Where the work happens | Methodology + remote signal (`fully remote Agile teams`) |
| `,` | Literal comma | Bridges to tail |
| **[Soft Skills Tail]** | Leadership / ownership / adaptability signal | Prepositional phrase starting with `with` (`with end-to-end ownership, mentorship, and fast adaptability across stacks, products, and industries`) |
| `.` | Single terminal period | The only period in the entire summary |

## Canonical Worked Example (v12)

This is a worked example from one senior full-stack/frontend engineer's CV. It is the **canonical reference filling** that the entire template was validated against. Match the structure exactly; replace the content with the candidate's own facts.

### English

> *Senior Full-Stack and Frontend Engineer* with 8+ years building scalable, high-performance e-commerce platforms, SaaS products, and consumer-facing web applications using *Vue 3*, *React*, *Next.js*, *TypeScript*, and *Node.js*, integrating frontend architecture, performance and conversion optimization, accessibility, SEO, and AI-assisted development in fully remote Agile teams, with end-to-end ownership, mentorship, and fast adaptability across stacks, products, and industries.

### Spanish

> *Ingeniero Full-Stack y Frontend Senior* con más de 8 años construyendo plataformas e-commerce, productos SaaS y aplicaciones web de cara al consumidor, escalables y de alto rendimiento, con *Vue 3*, *React*, *Next.js*, *TypeScript* y *Node.js*, integrando arquitectura frontend, optimización de rendimiento y conversión, accesibilidad, SEO y desarrollo asistido por IA en equipos Agile 100% remotos, con ownership end-to-end, mentoría y rápida adaptabilidad entre stacks, productos e industrias.

### Slot Filling Map for the Canonical Example

| Slot | EN content | ES content |
|---|---|---|
| Identity & Experience | `*Senior Full-Stack and Frontend Engineer* with 8+ years` | `*Ingeniero Full-Stack y Frontend Senior* con más de 8 años` |
| Core Output | `building scalable, high-performance e-commerce platforms, SaaS products, and consumer-facing web applications` | `construyendo plataformas e-commerce, productos SaaS y aplicaciones web de cara al consumidor, escalables y de alto rendimiento,` |
| using / con | `using` | `con` |
| Tech Stack | `*Vue 3*, *React*, *Next.js*, *TypeScript*, and *Node.js*` | `*Vue 3*, *React*, *Next.js*, *TypeScript* y *Node.js*` |
| Bridging Verb | `integrating` | `integrando` |
| Specialized Domains | `frontend architecture, performance and conversion optimization, accessibility, SEO, and AI-assisted development` | `arquitectura frontend, optimización de rendimiento y conversión, accesibilidad, SEO y desarrollo asistido por IA` |
| in / en | `in` | `en` |
| Environment | `fully remote Agile teams` | `equipos Agile 100% remotos` |
| Soft Skills Tail | `with end-to-end ownership, mentorship, and fast adaptability across stacks, products, and industries` | `con ownership end-to-end, mentoría y rápida adaptabilidad entre stacks, productos e industrias` |

## Adaptation to Other Surfaces

The recipe applies to professional summary paragraphs across multiple surfaces. Adapt the slot content but keep the structure:

| Surface | Adaptation notes |
|---|---|
| **CV header summary** (PDF) | Use the full template as-is. Italics via LaTeX `*text*`. |
| **LinkedIn About section** | Use the full template; LinkedIn renders inline. Bold via Unicode bold characters or drop bold entirely. Length: same 50-80 words. |
| **Personal site hero summary** | Use the full template; HTML `<strong>` for emphasis. |
| **Professional bio (~150 words)** | Use the recipe as the FIRST sentence; add one second sentence for personal context (location, availability beyond the summary signal, side projects). |
| **Other engineer's CV** | Replace [Identity & Experience], [Core Output], [Tech Stack], [Specialized Domains], [Environment], [Soft Skills Tail] with their own content. Template structure stays identical. |

## Examples

### Example A: Backend Engineer Adaptation

**Correct:** Filling the same template with backend-flavored content.

> *Senior Backend Engineer* with 10+ years building distributed systems, payment platforms, and high-throughput data pipelines using *Go*, *Python*, *Kafka*, *PostgreSQL*, and *Kubernetes*, combining systems architecture, performance optimization, observability, and security hardening in fully remote engineering teams, with end-to-end ownership, code review leadership, and fast adaptability across services, domains, and team sizes.

Why this works: every slot is filled with backend-domain content but the structure is unchanged.

### Example B: Wrong — Two Sentences

**Incorrect:** Breaking the template into multiple sentences.
> Senior Frontend Engineer with 8+ years of experience. Specialized in React, TypeScript, and Next.js. Strong focus on performance and accessibility.

Why this fails: violates the single-sentence mandate. The recipe is non-negotiable on this point.

**Correct:** Merge into one chained sentence.
> *Senior Frontend Engineer* with 8+ years building scalable web applications using *React*, *TypeScript*, and *Next.js*, integrating performance optimization and accessibility, in fully remote teams, with end-to-end ownership.

### Example C: Wrong — Verb-Led Second Half

**Incorrect:** Using a standalone verb to start the second half instead of a participle bridge.
> Senior Engineer with 8+ years building platforms using TypeScript and React. **Leads** architecture decisions and **drives** performance optimization.

Why this fails: violates the no-subject rule. "Leads" and "drives" are standalone verbs implying an unstated subject.

**Correct:** Use a participle bridge.
> *Senior Engineer* with 8+ years building platforms using *TypeScript* and *React*, **integrating** architecture decisions and performance optimization, in fully remote teams, with end-to-end ownership.

## Length Target

The completed sentence should run **50–80 words**. The canonical EN worked example is 62 words; the canonical ES is 70 words. Anything shorter likely drops a critical slot; anything longer is bloated and should be tightened by removing weakest [Specialized Domains] entries first.
