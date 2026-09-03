// Model.test.mjs — the widget's display math, checked without a shell.
//
// Model.js is Qt-free ON PURPOSE, and its own header says so: "every rule here
// can be reasoned about (and tested) without standing up a shell". This is that
// test. Everything else about the widget needs the running bar and its
// metrics()/diag() IPC — qmllint returns 255 silently on Panel.qml and proves
// nothing — but the pure functions do not, and those are where the arithmetic
// bugs live.
//
//   node Model.test.mjs
//
// `.pragma library` is a QML directive, not JavaScript, so it is stripped
// before evaluation. Nothing else about the file is transformed.

import { readFileSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"

const here = dirname(fileURLToPath(import.meta.url))
const src = readFileSync(join(here, "Model.js"), "utf8").replace(/^\.pragma library/m, "")
const M = {}
new Function("exports", src + "\n;Object.assign(exports, {calendarGrid, workRows, workWaitingCount, workButtonsFor, humanDuration, WEEKDAY_RAIL, calendarScaleText});")(M)

let pass = 0
const fails = []
function is(name, got, want) {
  const g = JSON.stringify(got), w = JSON.stringify(want)
  if (g === w) { pass++; console.log("  ok    " + name) }
  else { fails.push(name); console.log("  FAIL  " + name + "\n          want: " + w + "\n          got:  " + g) }
}
function head(t) { console.log("\n== " + t + " ==") }

// --- fixtures ---------------------------------------------------------------
// A window that STARTS MID-MONTH, which is the normal case and the one that was
// wrong: 24 whole weeks from a Sunday in late March.
function weeksFrom(startISO, n) {
  const out = []
  const d = new Date(startISO + "T00:00:00Z")
  for (let w = 0; w < n; w++) {
    const days = []
    for (let i = 0; i < 7; i++) {
      days.push({ date: d.toISOString().slice(0, 10), github: 0, commits: 0 })
      d.setUTCDate(d.getUTCDate() + 1)
    }
    out.push({ days })
  }
  return out
}
const activity = { weeks: weeksFrom("2026-03-22", 24), window: { today: "2026-09-02" } }

head("calendar: month labels read as a scale")

const g = M.calendarGrid(activity, 24)
const labelled = g.months.map((m, i) => (m ? i : -1)).filter((i) => i >= 0)

// THE FIX. March owns exactly two columns here (22 and 29 March), so it is
// drawn but not named — the same reason GitHub's leftmost columns are bare.
// Labelling it put "Mar" two columns from "Apr" while every other pair sat four
// or five apart, and labels that are not evenly spaced stop reading as a scale.
is("a month clipped by the window edge is not labelled",
   g.months[0], "")
is("the first label is the first month that fits",
   g.months[labelled[0]], "Apr")
is("and every gap between labels is a whole month",
   labelled.slice(1).map((v, i) => v - labelled[i]), [4, 5, 4, 4])
is("no month is labelled twice",
   g.months.filter((m) => m === "Apr").length, 1)

// A column belongs to the month of its FIRST day, as GitHub assigns them: the
// week of 31 May to 6 June is May's, so June starts at the column after it.
is("a week straddling a boundary belongs to the month it starts in",
   g.months[10] === "" && g.months[11] === "Jun", true)

// A full window has no clipped month at all, so nothing is dropped.
const full = M.calendarGrid({ weeks: weeksFrom("2026-03-01", 24), window: {} }, 24)
is("a window starting on the 1st labels its first month",
   full.months[0], "Mar")

head("calendar: the grid itself")

is("seven rows, one per weekday", g.rows.length, 7)
is("every row is as wide as the window", g.rows.map((r) => r.length), Array(7).fill(24))
is("the rail names Mon, Wed and Fri", M.WEEKDAY_RAIL, ["", "Mon", "", "Wed", "", "Fri", ""])
// A null is a day that has not happened. In a QML Row an invisible child is
// dropped from layout, so a null anywhere but the tail would shift that row
// sideways against every other row and against the month labels.
const strays = g.rows.flatMap((r, y) => r.map((c, x) => (c === null && x < 23 ? y + ":" + x : null)).filter(Boolean))
is("no gap falls anywhere but the last column", strays, [])

head("calendar: the legend says what a shade means")

// GitHub can get away with a bare Less/More because its scale is fixed for the
// year. Ours saturates at the 75th percentile of whatever window is drawn, so
// the same shade means different things at 12 weeks and at 52 — the number is
// what makes the legend worth having rather than decorative.
is("the legend names the count the darkest step is reached at",
   M.calendarScaleText(9), "9+ a day is darkest")
is("a busier window moves it", M.calendarScaleText(31), "31+ a day is darkest")
// A window with no activity at all has no scale to describe, and "0+ a day is
// darkest" would be nonsense printed under an empty grid.
is("an empty window says nothing", M.calendarScaleText(0), "")
is("and neither does a missing one", M.calendarScaleText(undefined), "")

head("work: sections, order and buttons")

const payload = {
  running: [
    { kind: "agent", id: "a1", repo: "bluespring", pr: 151, state: "running", done: 4, total: 15 },
    { kind: "run", id: "r1", repo: "kyo-utils", label: "run in flight" }
  ],
  waiting: [
    { kind: "comment", id: "m1", repo: "bluespring", pr: 151, title: "first" },
    { kind: "drift", id: "d1", repo: "subtitle-studio", title: "contract edited" },
    { kind: "conflict", id: "c1", repo: "bluespring-98", pr: 151, title: "will not merge" }
  ]
}
const rows = M.workRows(payload)

is("every row is present exactly once", rows.length, 5)
is("keys are unique, so the cursor cannot double-count",
   new Set(rows.map((r) => r.key)).size, rows.length)
// Most-decisive first: work already happening, then a frozen repo, then a
// blocked merge, then somebody waiting on a reply.
is("sections are ordered by what needs deciding first",
   rows.filter((r) => r.groupLabel).map((r) => r.groupLabel),
   ["Running now", "shipwright approve", "Conflicts", "Review threads"])
is("a heading is drawn by the first row of its section only",
   rows.map((r) => (r.groupLabel ? 1 : 0)), [1, 0, 1, 1, 1])

is("a running batch carries pause and stop", rows[0].buttons, ["pause", "stop"])
is("a holding one carries go and stop",
   M.workButtonsFor({ kind: "agent", state: "holding" }), ["go", "stop"])
// A rail bound to 0/0 would draw a full bar for work that has not started.
is("progress is a fraction of the batch", Math.round(rows[0].progress * 100), 27)
is("and is -1 when there is nothing to measure", rows[1].progress, -1)

is("the chip counts what waits on you, not what is running",
   M.workWaitingCount(payload), 3)

head("work: a whole pull request in one press")

// Fifteen threads on one PR are ONE piece of work — `shipwright agent --pr N`
// already takes the batch. Grouping only by kind left the heading with no
// button, so acting on the stack meant clicking a row, which starts a batch for
// that same PR anyway: fifteen clicks doing fifteen times the same thing.
const twoPrs = M.workRows({ running: [], waiting: [
  { kind: "comment", id: "m1", repo: "bluespring", pr: 151, title: "a" },
  { kind: "comment", id: "m2", repo: "bluespring", pr: 151, title: "b" },
  { kind: "comment", id: "m3", repo: "other", pr: 9, title: "c" }
] }, [])
const heads = twoPrs.filter((r) => r.groupLabel)

is("threads split into one group per pull request", heads.length, 2)
is("and each heading says which one", heads.map((h) => h.groupSub),
   ["bluespring #151", "other #9"])
is("the heading carries the batch buttons", heads[0].groupButtons, ["agent", "read"])
is("it knows the pull request they act on", heads[0].groupPr, "151")
is("a two-thread group counts two", heads[0].groupCount, 2)
is("only the first row of a group carries them",
   twoPrs.map((r) => r.groupButtons.length), [2, 0, 2])

// A drifted contract has no pull request, so batching it would mean nothing.
const drift = M.workRows({ running: [], waiting: [
  { kind: "drift", id: "d1", repo: "subtitle-studio", title: "edited" }
] }, [])
is("a row with no pull request gets no batch button", drift[0].groupButtons, [])
is("and no sub-label to explain one", drift[0].groupSub, "")

head("work: running a repo from the bar")

const repos = [
  { name: "shipwright", uncommittedN: 54, aheadN: 0, paused: true,  armed: true },
  { name: "kyo-utils",  uncommittedN: 3,  aheadN: 1, paused: false, armed: true },
  { name: "quiet",      uncommittedN: 0,  aheadN: 0, paused: false, armed: true },
  { name: "gone",       uncommittedN: 9,  aheadN: 0, paused: false, armed: true, missing: true }
]
const withRepos = M.workRows({ running: [], waiting: [] }, repos)

// Only repos with something to do. Listing all ten would put a permanent
// ten-row section under a tab whose whole premise is that everything on it
// needs you.
is("a repo with nothing to do is not listed",
   withRepos.some((r) => r.repo === "quiet"), false)
is("nor is one whose path is gone",
   withRepos.some((r) => r.repo === "gone"), false)
is("a repo with work offers a real run and a dry one",
   withRepos.find((r) => r.repo === "kyo-utils").buttons, ["run", "dry"])
is("and says what there is to do",
   withRepos.find((r) => r.repo === "kyo-utils").detail, "3 uncommitted \u00b7 1 unpushed")

// A paused repo cannot run at all, so `run` there would be a button that does
// nothing. This is not hypothetical: shipwright itself is paused right now.
is("a paused repo offers only the button that unblocks it",
   withRepos.find((r) => r.repo === "shipwright").buttons, ["resume"])
is("and says that is why", withRepos.find((r) => r.repo === "shipwright").detail,
   "54 uncommitted \u00b7 paused")
is("the section is headed once", withRepos[0].groupLabel, "Ready to run")

// Repos sort above the blocked kinds: something you can act on outranks
// something you are waiting on.
const mixed = M.workRows({ running: [], waiting: [
  { kind: "comment", id: "m1", repo: "bluespring", pr: 151, title: "a" }
] }, repos)
is("ready-to-run comes before review threads",
   mixed.filter((r) => r.groupLabel).map((r) => r.groupLabel),
   ["Ready to run", "Review threads"])

console.log("\n" + pass + " passed, " + fails.length + " failed")
if (fails.length) { fails.forEach((f) => console.log("  - " + f)); process.exit(1) }
