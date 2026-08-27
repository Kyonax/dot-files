.pragma library

// Pure display maths. No QML types, no I/O, no side effects — so the awkward
// parts (what does the bar say when a period is locked but a draft exists?) can
// be reasoned about, and later tested, without standing up a shell.

function hours(m) {
    if (!m) return "0h"
    var h = m / 60
    return (Math.abs(h - Math.round(h)) < 0.01 ? Math.round(h) : h.toFixed(1)) + "h"
}

// The three states every widget in this bar speaks. Only `alert` earns the
// theme's active colour; a settled tool should not pull the eye.
function state(d) {
    if (!d || !d.ok) return "alert"
    if (d.paused) return "alert"
    if (d.periodLocked) return "alert"
    if (d.lastOutcome === "vetoed" || d.lastOutcome === "partial") return "alert"
    if (d.gapMinutes > 0 || d.draftMinutes > 0) return "pending"
    return "settled"
}

// Owed hours plus state, which is what the owner asked the bar to answer:
// "do I owe Tempo anything right now?"
function barText(d) {
    if (!d || !d.ok) return "—"
    if (d.paused) return "PAUSED"
    if (d.periodLocked) return "LOCKED"
    if (d.lastOutcome === "vetoed") return "STOPPED"
    if (d.gapMinutes > 0) return hours(d.gapMinutes) + " owed"
    if (d.draftMinutes > 0) return hours(d.draftMinutes) + " drafted"
    return "0h"
}

function subtitle(d) {
    if (!d || !d.ok) return "tempo-hours is not answering"
    if (d.paused) return "paused — nothing will run"
    if (d.periodLocked) return "the target period is not OPEN"
    if (d.gapMinutes > 0)
        return d.dayCount + " working day(s) unlogged"
    if (d.draftMinutes > 0)
        return "drafted, waiting to be pushed"
    return "Tempo is square"
}

function armedLabel(d) {
    return d && d.armed ? "armed" : "disarmed — drafts only"
}

function parse(text) {
    var d = { ok: false, gapMinutes: 0, draftMinutes: 0, dayCount: 0,
              days: [], themes: [], armed: false, paused: false,
              periodLocked: false, lastOutcome: "", fetched: false, periods: [],
              capturesStored: 0, capturesTotal: 0, ageSeconds: null }
    if (!text) return d
    var j
    try { j = JSON.parse(text) } catch (e) { return d }
    d.ok = true
    d.fetched = j.fetched !== false
    d.ageSeconds = (j.age_seconds === undefined) ? null : j.age_seconds
    d.armed = !!j.armed
    d.paused = !!j.paused
    d.gapMinutes = Math.round((j.gap_h || 0) * 60)
    d.dayCount = j.day_count || (j.days ? j.days.length : 0)

    if (j.days && j.days.length && j.days[0].required_h !== undefined) {
        d.days = j.days.map(function (x) {
            return { date: x.date, label: x.weekday || "", minutes: Math.round(x.gap_h * 60),
                     thin: false }
        })
    } else if (j.draft && j.draft.days) {
        d.days = j.draft.days.map(function (x) {
            return { date: x.date, label: "", minutes: x.total_minutes, thin: !!x.thin }
        })
    }
    if (j.draft) {
        d.draftMinutes = j.draft.total_minutes || 0
        d.themes = (j.draft.themes || []).map(function (t) {
            return { text: t.description, minutes: t.budget_minutes }
        })
    }
    if (j.themes) {
        d.themes = j.themes.map(function (t) {
            return { text: t.description, minutes: t.budget_minutes }
        })
    }
    if (j.last) d.lastOutcome = j.last.outcome || ""
    d.periods = j.periods || []
    // The cheap read has no day_count of its own — derive it from the rows we
    // actually built, or the hero claims "across 0 working days".
    if (!d.dayCount) d.dayCount = d.days.length
    if (j.captures) {
        d.capturesStored = j.captures.stored || 0
        d.capturesTotal = j.captures.total || 0
    }
    if (j.periods)
        d.periodLocked = j.periods.some(function (p) { return p.status !== "OPEN" })
    if (j.state === "alert") d.periodLocked = d.periodLocked || (d.lastOutcome === "")
    return d
}

// A one-line summary of the run behind the number, for the hero's meta line.
function runText(d) {
    if (!d || !d.ok) return "tempo-hours is not answering"
    if (d.paused) return "paused — nothing will run"
    if (d.gapMinutes > 0)
        return hours(d.gapMinutes) + " unlogged across " + d.dayCount + " working day"
             + (d.dayCount === 1 ? "" : "s")
    if (d.draftMinutes > 0) return hours(d.draftMinutes) + " drafted, waiting to be pushed"
    return "Tempo is square"
}

// A day earns attention only when it has no evidence behind it. Everything else
// is ordinary, and ordinary should not be coloured.
function dayState(day) {
    if (!day) return "settled"
    return day.thin ? "alert" : "pending"
}

function periodText(d) {
    if (!d || !d.ok) return "—"
    if (!d.periods || !d.periods.length) return "none in range"
    return d.periods.map(function (p) { return p.month + " " + p.status }).join(", ")
}

// Nightly captures freeze a day's evidence before it decays. What matters in
// the bar is whether any day is still uncaptured, because those are the ones
// the weekly run will have to re-derive from evidence that has moved.
function capturesText(d) {
    if (!d || !d.ok) return "—"
    // The cached payload carries the denominator too, so a replayed read is no
    // less informative than a live one — it is only older.
    if (!d.capturesTotal) return d.capturesStored ? d.capturesStored + " stored"
                                                  : "nothing to capture yet"
    if (d.capturesStored >= d.capturesTotal) return d.capturesStored + "/" + d.capturesTotal + " — complete"
    return d.capturesStored + "/" + d.capturesTotal + " — " + (d.capturesTotal - d.capturesStored) + " missing"
}

function capturesIncomplete(d) {
    return !!(d && d.ok && d.fetched && d.capturesTotal && d.capturesStored < d.capturesTotal)
}

// How old the numbers are. The cheap read replays the last full payload, so the
// panel must be able to say "this is from four minutes ago" rather than quietly
// presenting stale figures as current.
function ageText(d) {
    if (!d || !d.ok) return ""
    if (d.state === "unknown") return "never checked — press refresh"
    if (d.fetched) return "just now"
    if (d.ageSeconds === null || d.ageSeconds === undefined) return ""
    var s = d.ageSeconds
    if (s < 90) return "moments ago"
    if (s < 5400) return Math.round(s / 60) + " min ago"
    if (s < 172800) return Math.round(s / 3600) + "h ago"
    return Math.round(s / 86400) + "d ago"
}

function stale(d) {
    return !!(d && d.ok && !d.fetched && d.ageSeconds !== null && d.ageSeconds > 1800)
}
