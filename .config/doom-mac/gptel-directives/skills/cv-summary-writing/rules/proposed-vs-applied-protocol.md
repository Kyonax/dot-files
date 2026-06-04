---
title: The Proposed-vs-Applied Protocol for Iterative CV Summary Edits
impact: HIGH
impactDescription: Protocol for collaborative CV summary refinement between an AI drafter and a human reviewer. Without this protocol, AI overcorrects on user feedback (especially "almost there") into wholesale rewrites that destroy what was working — turning a 3-iteration job into a 12-iteration job.
tags: cv, summary, protocol, iteration, refinement, almost-there, present-decide-execute, user-approval, overcorrection, collaborative-editing
---

This rule defines the editing protocol for collaborative CV summary refinement. The protocol exists because a real failure mode was observed during a 12-iteration drafting journey: when the user said "almost there" after one draft, the AI did a wholesale rewrite and the next draft was "worse than the beginning". Recovery required reverting and re-iterating surgically. This protocol prevents that failure.

## The Protocol (5 Steps)

For every CV summary edit affecting the canonical text:

```
1. PRESENT the proposed text first, with a clear diff vs. the current version
2. WAIT for explicit user approval (e.g., "yes", "go", "apply", "✓", "winner")
3. EXECUTE the edit only after approval — never speculatively
4. NO BATCHING — do not stack multiple drafts without checkpointing
5. NO OVERCORRECTION — if user says "almost there", refine surgically not wholesale
```

## Rule 1 — Present, Don't Apply

**Always show the proposed text in the conversation before editing any file.** The AI's role is to PROPOSE; the user's role is to APPROVE.

| Action | Allowed? |
|---|---|
| Draft a new version inline in the chat | ✓ Yes — always |
| Show a diff/comparison table inline | ✓ Yes — preferred for visibility |
| Run the Edit tool on the canonical file | ✗ Only after explicit user approval |
| Run multiple Edit tool calls in parallel without approval | ✗ Never |

**Why:** The user is the final arbiter of voice and tone. Even a "correct per the recipe" draft may fail user review. Edit-then-revert wastes file history and creates noise in version control.

## Rule 2 — Wait for Explicit Approval

| User signal | Action |
|---|---|
| `"yes"`, `"go"`, `"apply"`, `"do it"`, `"ship it"`, `"winner"` | ✓ Apply the proposed edit |
| `"better"`, `"closer"`, `"almost there"` | ⚠️ Iterate — do NOT apply yet |
| `"hmm"`, `"not sure"`, `"interesting"` | ⚠️ Ask a clarifying question — do NOT apply |
| No response after presenting | ⚠️ Wait — don't assume |
| Negative ("no", "still off", "still awful") | ⏪ Revert mentally — don't apply |

**Approval must be unambiguous.** "Looks fine" or "I guess" are not approvals — they signal lukewarm acceptance, which often becomes a regression complaint later. When in doubt, ask: "Do you want me to apply this, or iterate?"

## Rule 3 — Execute Only After Approval

When approved, execute promptly:

```
User: "That is the winner!"
AI:    [Edit tool call against the canonical file]
AI:    "Applied. The summary now reads: [confirms final text]"
```

Do NOT add unrelated changes during execution. If the user approved version X, apply exactly version X — not version X plus an extra fix.

## Rule 4 — No Batching

| Anti-pattern | Better approach |
|---|---|
| Present 3 alternative drafts in one message, then apply whichever the user picks | Present ONE draft; if rejected, ask for direction before drafting the next |
| Apply 5 edits to the org file in parallel covering 5 different concerns | One approved edit at a time; checkpoint after each |
| Combine summary edit + role description edit + filename rename in one batch | Split into 3 separate Propose-Approve-Apply cycles |

**Why:** Batching multiple edits without checkpoints means if any single one is wrong, the user has to specify which exact thing to revert. Single-edit cycles are slower but produce clean partial wins.

## Rule 5 — No Overcorrection (The "Almost There" Rule)

**When the user says "almost there", they want SURGICAL refinement, not a wholesale rewrite.**

This is the most important rule in the protocol. Misreading "almost there" as "still bad, try something new" causes the worst regressions.

| User signal | What they mean | What to do |
|---|---|---|
| `"Almost there"` | Direction is right; ONE specific thing is wrong | Surgical fix on the flagged phrase ONLY; preserve everything else |
| `"You're almost to nail it but [phrase X] feels [Y]"` | Refine ONLY phrase X | Surgical fix on phrase X; preserve the rest verbatim |
| `"Better but [phrase X] is still off"` | Direction good, phrase X still needs work | Surgical fix on X only |
| `"Still awful"` / `"Worse than before"` | Wholesale rewrite went wrong; the previous draft was closer | Revert mentally to the previous draft; iterate surgically from there |
| `"I don't know what I want"` + `"this is awful"` | Drafting cycle is broken | STOP drafting. Offer 4 working methods (see below) |

### The "I Don't Know What I Want" Working Methods

When the user signals frustration AND uncertainty in the same message, STOP presenting more drafts. The drafting cycle has broken down. Offer these 4 working methods instead:

| Method | Description | When best |
|---|---|---|
| **1. Reference** | Ask user to paste 1-2 sample summaries from CVs they respect (peers, role models). Match that voice. | When the user has a voice in mind they can't articulate |
| **2. User-writes-rough** | User drafts a rough version (even bullet points). AI cleans up and ATS-optimizes. | When the user knows the content but struggles with structure |
| **3. Strip-down** | Reduce summary to minimum viable form (Identity + Years + Stack + Remote signal). Cut all middle slots. | When the user is overwhelmed by complexity |
| **4. Park** | Revert to the pre-session state. Come back fresh. | When the user is frustrated and needs a break |

Present these as 4 distinct options; let the user pick. Do NOT pick one and proceed without their explicit choice.

## Failure Mode Documentation

The following is a recorded example of the overcorrection failure mode. Future AIs working on summary edits must recognize this pattern.

### The Sequence

| Iteration | What happened | User signal |
|---|---|---|
| v3 | AI drafted: "Each project moves from architecture decisions through Lighthouse and Core Web Vitals..." | "Almost there" — flagged the "moves from X through Y" phrase as artificial |
| v4 (overcorrection) | AI did wholesale rewrite: "In practice, that means architecture decisions made upfront..." | Rejected as "worse than before" |
| v5 (further overcorrection) | AI did another wholesale rewrite: "Architecture is decided upfront, performance is established..." (passive three-clauses) | Rejected as "still awful" |
| Recovery | User said "the previous one was almost there" referring to v3. AI reverted to v3 base and made ONLY the surgical fix. | Convergence resumed |

### The Lesson

When the user says "almost there":
- ✓ DO touch ONLY the specific phrase they flagged
- ✓ DO preserve everything else verbatim
- ✗ DON'T rewrite sentences they didn't flag
- ✗ DON'T introduce new structures
- ✗ DON'T "improve the surrounding context"

## Correct vs. Incorrect Protocol Execution

### Example A — Skipping Approval

**Incorrect:**
```
User: "Try a version with mentorship added"
AI:   [Immediately runs Edit tool to update the org file]
AI:   "I added mentorship. The summary now reads..."
```

The user said "try a version" — that's a request for a DRAFT, not an instruction to apply.

**Correct:**
```
User: "Try a version with mentorship added"
AI:   "Here's the proposed v12 with mentorship added: [...draft...]
       Apply this, or iterate?"
User: "Apply it"
AI:   [Runs Edit tool]
```

### Example B — Overcorrecting on "Almost There"

**Incorrect:**
```
User: "Almost there but 'fully remote Agile teams' feels weak"
AI:   "Let me try a completely different approach with the entire second half restructured..."
      [Drafts version with totally new bridge, new domain order, new tail]
```

Wholesale rewrite — destroys what was working.

**Correct:**
```
User: "Almost there but 'fully remote Agile teams' feels weak"
AI:   "Surgical fix on the [Environment] slot only. Other options for that slot:
       - fully remote distributed teams
       - fully remote cross-functional teams
       - remote-first Agile environments
       Which lands better? Everything else stays."
```

### Example C — Batching Without Checkpoints

**Incorrect:**
```
User: "Apply v12 and also rename the PDFs and also update the snippets"
AI:   [Runs Edit on org file, then mv on PDFs, then Edit on snippets.js, all in parallel]
AI:   "Done with all three"
```

If any one fails or the user wanted a different version of any sub-task, recovery is messy.

**Correct:**
```
User: "Apply v12 and also rename the PDFs and also update the snippets"
AI:   "I'll do these as separate cycles. First — applying v12 to both org files.
       Confirming the exact text: [...v12 EN + ES...]. Apply this?"
User: "Yes"
AI:   [Edit on org files; confirms applied]
AI:   "Next — PDF rename. Proposing: cv_*.pdf → Cristian-Moreno-Senior-Software-Engineer-{EN,ES}.pdf
       and download attr → Cristian-Moreno-Senior-Software-Engineer-CV.pdf (locale-agnostic).
       Apply this?"
User: "Go"
AI:   [git mv + hero.vue edits; confirms applied]
AI:   "Snippets next — but the descriptions need careful review per role. Want me to
       start with agile-engine EN, or pick a different role first?"
```

Three separate cycles, three checkpoints. If any one is wrong, only that one rolls back.
