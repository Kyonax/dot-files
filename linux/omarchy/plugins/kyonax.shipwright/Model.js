// Pure display math for the shipwright bar plugin. Qt-free on purpose, the way
// omarchy's own KeyboardLayoutModel.js and dropbox/Model.js are, so every rule
// here can be reasoned about (and tested) without standing up a shell.
//
// `shipwright health --json` is the single source of truth. Nothing in this
// file invents state; it only decides how the state should read.

.pragma library

// ---------------------------------------------------------------- parsing

// Declared here because parseHealth sorts by it, and a `var` initialiser does
// not hoist with the declaration.
var TIER_ORDER = ["always", "guarded", "client"]


function parseHealth(raw) {
  var text = String(raw || "").trim()
  if (text === "") return { ok: false, lastError: "shipwright health returned nothing" }

  var data
  try {
    data = JSON.parse(text)
  } catch (e) {
    return { ok: false, lastError: "could not parse shipwright health output" }
  }
  if (!data || typeof data !== "object") {
    return { ok: false, lastError: "unexpected shipwright health payload" }
  }

  // ORDERED BY TIER, not alphabetically.
  //
  // The fleet draws a tier header whenever a row's tier differs from the one
  // above it. The CLI returns repos sorted by name, and on this queue the tiers
  // alternate almost every row — so an unsorted list produced eight headers for
  // three tiers and 420 px of fleet where 280 was budgeted. Sorting here means
  // every consumer (the list, the cursor, the headers) shares one order without
  // any of them having to know.
  var repos = Array.isArray(data.repos) ? data.repos.map(normalizeRepo) : []
  repos.sort(function (a, b) {
    var ta = TIER_ORDER.indexOf(a.tier), tb = TIER_ORDER.indexOf(b.tier)
    if (ta === -1) ta = TIER_ORDER.length
    if (tb === -1) tb = TIER_ORDER.length
    if (ta !== tb) return ta - tb
    return a.name < b.name ? -1 : (a.name > b.name ? 1 : 0)
  })
  var notes = Array.isArray(data.notes) ? data.notes.map(String) : []
  var prs = Array.isArray(data.prs) ? data.prs.map(normalizePr) : []

  return {
    ok: true,
    run: data.run || null,
    repos: repos,
    notes: notes,
    prs: prs,
    prsMine: toInt(data.prs_mine),
    attention: Array.isArray(data.attention) ? data.attention.map(normalizeAttention) : [],
    nextSlot: data.next_slot || null,
    running: data.running || null,
    lastError: ""
  }
}

// One item per repo that needs a person. The CLI already decided WHICH repos
// those are and what the one thing to do about each is (sw_outcome_action), so
// this file must not form a second opinion about either — the bar and the
// terminal giving different advice about the same repo is the failure the
// shared attention table exists to prevent.
function normalizeAttention(a) {
  var it = a || {}
  return {
    repo: String(it.repo || "?"),
    tier: String(it.tier || ""),
    outcome: String(it.outcome || ""),
    kind: String(it.kind || ""),
    runs: toInt(it.runs),
    reason: String(it.reason || ""),
    action: String(it.action || ""),
    sinceAt: String(it.since_at || ""),
    source: String(it.source || "")
  }
}

// ------------------------------------------------------------ pull requests
//
// A PR raised in a night slot is deliberately NOT opened in a browser (nine
// tabs while you sleep is worse than none), so the panel is where the morning
// finds out about it. `mine` means shipwright raised it, which is the case that
// matters most: `restore_branch` already put the checkout back, so that work is
// on its branch and NOT in the working tree until the PR merges.

function normalizePr(p) {
  var pr = p || {}
  return {
    repo: String(pr.repo || "?"),
    number: toInt(pr.number),
    title: String(pr.title || ""),
    url: String(pr.url || ""),
    branch: String(pr.branch || ""),
    author: String(pr.author || "?"),
    createdAt: String(pr.created_at || ""),
    draft: pr.draft === true,
    mine: pr.mine === true
  }
}

// Compact age. "4d" is read at a glance; a timestamp is not.
function prAgeText(pr) {
  if (!pr || !pr.createdAt) return ""
  var then = Date.parse(pr.createdAt)
  if (!isFinite(then)) return ""
  var mins = Math.max(0, Math.floor((Date.now() - then) / 60000))
  if (mins < 60) return mins + "m"
  if (mins < 1440) return Math.floor(mins / 60) + "h"
  return Math.floor(mins / 1440) + "d"
}

function prTitleText(pr) {
  if (!pr) return ""
  return (pr.draft ? "[draft] " : "") + pr.title
}

// repo, number, age and who — everything except the title, which gets its own
// line because it is the part worth reading.
function prDetailText(pr) {
  if (!pr) return ""
  var bits = [pr.repo + " #" + pr.number]
  var age = prAgeText(pr)
  if (age !== "") bits.push(age)
  bits.push(pr.mine ? "shipwright" : pr.author)
  return bits.join(" · ")
}

function prTooltip(pr) {
  if (!pr) return ""
  if (pr.mine) {
    return pr.repo + " #" + pr.number + " — raised by shipwright.\n"
         + "This work is on " + pr.branch + ", not in your working tree.\n"
         + "Click to open on GitHub."
  }
  return pr.repo + " #" + pr.number + " by " + pr.author + "\nClick to open on GitHub."
}

// The one line the section header carries. Says whether anything is waiting on
// the owner specifically, not just how many PRs exist.
function prsSummaryText(prs, prsMine) {
  var total = Array.isArray(prs) ? prs.length : 0
  if (total === 0) return ""
  var mine = toInt(prsMine)
  if (mine > 0) return mine + " waiting on your merge · " + total + " open"
  return total + " open"
}

function normalizeRepo(r) {
  var repo = r || {}
  // ahead/behind/uncommitted arrive as strings from the CLI; keep the string for
  // display but carry a number so the badges can do arithmetic.
  return {
    name: String(repo.name || "unknown"),
    // The editor action needs somewhere to open. Emitted by _health_row_json.
    path: String(repo.path || ""),
    branch: String(repo.branch || ""),
    ahead: String(repo.ahead || "0"),
    behind: String(repo.behind || "0"),
    uncommitted: String(repo.uncommitted || "0"),
    aheadN: toInt(repo.ahead),
    behindN: toInt(repo.behind),
    uncommittedN: toInt(repo.uncommitted),
    outcome: String(repo.outcome || ""),
    missing: repo.missing === true,
    reason: String(repo.reason || ""),
    // Everything below arrives only from a shipwright that knows about tiers.
    // An older CLI leaves them empty and every consumer below falls back, so
    // the widget never has to know which version it is talking to.
    tier: String(repo.tier || ""),
    tierSource: String(repo.tier_source || ""),
    identity: String(repo.identity || ""),
    armed: repo.armed === true,
    paused: repo.paused === true,
    drift: String(repo.drift || ""),
    attention: repo.attention || null,
    attentionRuns: repo.attention ? toInt(repo.attention.runs) : 0,
    attentionKind: repo.attention ? String(repo.attention.kind || "") : "",
    attentionAction: repo.attention ? String(repo.attention.action || "") : ""
  }
}

function toInt(v) {
  var n = parseInt(String(v === undefined || v === null ? "0" : v), 10)
  return isFinite(n) ? n : 0
}

// ------------------------------------------------------------ repo state
//
// Three levels, because the bar only needs three. `alert` is anything a human
// has to decide about (a veto, a failure, a repo that vanished). `pending` is
// work that exists but is not yet a problem -- unpushed commits, uncommitted
// files, a repo that is behind. `settled` is everything else.

function repoState(repo) {
  if (!repo) return "settled"
  if (repo.missing) return "alert"

  // The CLI's own classification wins where it exists. A deferral that a retry
  // will clear by itself is NOT an alert — the bar turning urgent because
  // somebody was typing four minutes ago is the noise that trains people to
  // stop reading it.
  if (repo.attentionKind === "transient") return "pending"
  if (repo.attentionKind === "needs-input" || repo.attentionKind === "hard") return "alert"

  if (repo.drift === "drift" || repo.drift === "gone" || repo.drift === "missing") return "alert"
  var outcome = String(repo.outcome || "").toLowerCase()
  if (outcome === "vetoed" || outcome === "failed" || outcome === "error") return "alert"
  if (repo.reason !== "") return "alert"
  if (repo.aheadN > 0 || repo.behindN > 0 || repo.uncommittedN > 0) return "pending"
  return "settled"
}

// A paused or disarmed repo is not settled and not broken: it is switched off,
// which the row says in words and the dot says by going dim.
function repoIsOff(repo) {
  return !!repo && (repo.paused === true || repo.armed === false)
}

function repoOutcomeText(repo) {
  if (!repo) return ""
  if (repo.missing) return "missing"
  return repo.outcome || "—"
}

// The line under the repo name. Reads as prose rather than a column dump,
// because the popup is not the TUI and does not need to be a table.
function repoDetailText(repo) {
  if (!repo) return ""
  if (repo.missing) return "path does not exist"

  var parts = []
  if (repo.branch !== "") parts.push(repo.branch)
  if (repo.aheadN > 0) parts.push(repo.ahead + " ahead")
  if (repo.behindN > 0) parts.push(repo.behind + " behind")
  if (repo.uncommittedN > 0) parts.push(repo.uncommitted + " uncommitted")
  if (parts.length === 1 && repo.branch !== "") parts.push("in sync")
  return parts.join(" · ")
}

// A hover has to ADD something. Returning the reason meant hovering a row
// reproduced the row — the same sentence, in a box, on top of its neighbour —
// which is a tooltip that has nothing to say. The row shows the reason; the
// tooltip shows what the row had no width for: the command, and the git state
// the reason displaced.
function repoTooltip(repo) {
  if (!repo) return ""
  if (repo.missing) return repo.name + " — repo path does not exist (disk unmounted?)"

  // The tooltip is the row's OVERFLOW, in the row's own order: the git state in
  // full (the detail column elides it — `docs/105-payee-is-a-customer-…` loses
  // the uncommitted count exactly when that count matters), then the two things
  // the row has no column for at all.
  var bits = []
  var detail = repoDetailText(repo)
  if (detail !== "") bits.push(detail)
  if (repo.reason !== "") bits.push(repo.reason)
  if (repo.attentionAction !== "") bits.push(repo.attentionAction)
  if (bits.length === 0) return ""
  return repo.name + " — " + bits.join("\n")
}

// ------------------------------------------------------------ fleet state

function fleetState(repos) {
  var list = repos || []
  var worst = "settled"
  for (var i = 0; i < list.length; i++) {
    var s = repoState(list[i])
    if (s === "alert") return "alert"
    if (s === "pending") worst = "pending"
  }
  return worst
}

function countByState(repos, state) {
  var list = repos || []
  var n = 0
  for (var i = 0; i < list.length; i++) if (repoState(list[i]) === state) n++
  return n
}

// --------------------------------------------------------------- the run

function runText(run) {
  if (!run) return "no runs recorded yet"
  if (run.in_progress === true) return "run " + String(run.slot || "") + " in progress"
  var processed = toInt(run.processed)
  var deferred = toInt(run.deferred)
  var failed = toInt(run.failed)
  var bits = [processed + " processed"]
  if (deferred > 0) bits.push(deferred + " deferred")
  if (failed > 0) bits.push(failed + " failed")
  return "run " + String(run.slot || "—") + " · " + bits.join(", ")
}

function runSlotText(run) {
  if (!run) return "never"
  var slot = String(run.slot || "")
  var elapsed = String(run.elapsed || "")
  if (run.in_progress === true) return slot + " (running)"
  return elapsed !== "" ? slot + " · took " + elapsed : (slot || "never")
}

function fleetSummaryText(repos) {
  var list = repos || []
  if (list.length === 0) return "no repos enrolled"
  var alerts = countByState(list, "alert")
  var pending = countByState(list, "pending")
  if (alerts > 0) return alerts + " of " + list.length + " need a decision"
  if (pending > 0) return pending + " of " + list.length + " have pending work"
  return "all " + list.length + " settled"
}

// ------------------------------------------------------------ tier groups
//
// The fleet page groups by TIER because that is the decision the queue is
// organised around: `always` is the owner's own work, where a day with no
// commit is a day lost; `client` is somebody else's repository, where the
// ceremony is the point. A flat alphabetical list buries that.
//
// An older shipwright sends no tier at all, so everything lands in one
// unlabelled group rather than in a group called "unknown".


function tierGroups(repos) {
  var list = repos || []
  var buckets = {}
  var untiered = []

  for (var i = 0; i < list.length; i++) {
    var t = String(list[i].tier || "")
    if (TIER_ORDER.indexOf(t) === -1) { untiered.push(list[i]); continue }
    if (!buckets[t]) buckets[t] = []
    buckets[t].push(list[i])
  }

  var out = []
  for (var k = 0; k < TIER_ORDER.length; k++) {
    var tier = TIER_ORDER[k]
    if (!buckets[tier] || buckets[tier].length === 0) continue
    out.push(tierGroup(tier, tier, buckets[tier]))
  }
  if (untiered.length > 0) out.push(tierGroup("", "repositories", untiered))
  return out
}

function tierGroup(tier, label, repos) {
  var settled = 0
  for (var i = 0; i < repos.length; i++) {
    if (repoState(repos[i]) === "settled" && !repoIsOff(repos[i])) settled++
  }
  return {
    tier: tier,
    label: label,
    repos: repos,
    settled: settled,
    total: repos.length,
    summary: settled + " of " + repos.length + " settled"
  }
}

// ------------------------------------------------------------- attention
//
// The flat list the Attention page renders, worst first. It comes from the
// CLI's own `attention` array where that exists; a shipwright too old to send
// one is not silently reported as clean — the repos are derived from the same
// rows the fleet page already shows.
function attentionItems(attention, repos) {
  var list = Array.isArray(attention) ? attention : []
  if (list.length > 0) {
    return list.slice().sort(function (a, b) { return b.runs - a.runs })
  }

  var out = []
  var rs = repos || []
  for (var i = 0; i < rs.length; i++) {
    if (repoState(rs[i]) !== "alert") continue
    out.push({
      repo: rs[i].name, tier: rs[i].tier, outcome: repoOutcomeText(rs[i]),
      kind: "needs-input", runs: rs[i].attentionRuns,
      reason: rs[i].reason || repoDetailText(rs[i]),
      action: rs[i].attentionAction, sinceAt: "", source: ""
    })
  }
  return out
}

// Eleven repos frozen by the same edited contract drew eleven identical rows,
// every one of them truncating the same sentence mid-word. The list said
// "something is wrong" eleven times and never once said what to do about it.
//
// Group by the COMMAND that clears it. The command is stated once, as a heading
// with its count, and the repos it applies to are listed under it — one row
// each, carrying the thing the heading cannot: which repo, and what state it is
// actually in. That is the shape somebody clearing the list works in: run this
// one command, these are the arguments.
function attentionGroups(items) {
  var list = Array.isArray(items) ? items : []
  var order = []
  var by = {}
  for (var i = 0; i < list.length; i++) {
    var it = list[i]
    // Keyed on the command, not the wording of the reason: two repos told to
    // run `shipwright approve` belong together whatever sentence explained it.
    var key = groupAction([it]) === "" ? ("?" + String(it.reason || ""))
                                       : String(it.outcome || "")
    if (by[key] === undefined) {
      by[key] = { key: key, cause: String(it.reason || ""), items: [] }
      order.push(key)
    }
    by[key].items.push(it)
  }
  var out = []
  for (var k = 0; k < order.length; k++) {
    var g = by[order[k]]
    g.count = g.items.length
    g.repos = g.items.map(function (x) { return String(x.repo || "") })
    g.action = groupAction(g.items)
    out.push(g)
  }
  return out
}

// One command for the whole group. Where the command names a repo, the name
// becomes a placeholder rather than one arbitrary member's: "shipwright approve
// <repo>" is true of all nine, "shipwright approve brain" is true of one.
function groupAction(items) {
  var first = String(items[0].action || "")
  if (first === "") return ""
  var name = String(items[0].repo || "")
  if (items.length === 1) return first
  if (name !== "" && first.indexOf(name) >= 0) return first.split(name).join("<repo>")
  return first
}

// How many repos of a group are listed before it folds. Three is what keeps two
// or three groups inside the tab without scrolling; the rest are one click away.
var ATTENTION_HEAD = 3

// The flat list of rows actually drawn. The panel walks THIS with j/k — a cursor
// over the grouped list and a cursor over the ungrouped one would disagree about
// what row 4 is.
//
// One row per REPO, not per group: every repo keeps its own click target, its
// own state, and its own report. The command is a heading carried by the first
// row of its group, the way a tier header is carried by the first row of its
// tier — so the cursor still walks one flat list of rows.
function attentionRows(groups, expanded) {
  var gs = Array.isArray(groups) ? groups : []
  var ex = Array.isArray(expanded) ? expanded : []
  var open = {}
  for (var e = 0; e < ex.length; e++) open[String(ex[e])] = true

  var out = []
  for (var i = 0; i < gs.length; i++) {
    var g = gs[i]
    var isOpen = open[g.key] === true
    var shown = isOpen ? g.count : Math.min(ATTENTION_HEAD, g.count)
    for (var j = 0; j < shown; j++) {
      out.push({
        kind: "item",
        key: g.key + "#" + j,
        groupKey: g.key,
        // The command heads its group, drawn by the first row of it.
        groupLabel: j === 0 ? g.action : "",
        groupCount: j === 0 ? g.count : 0,
        label: g.repos[j],
        reason: String(g.items[j].reason || ""),
        action: String(g.items[j].action || ""),
        count: 1,
        expanded: isOpen,
        target: g.repos[j],
        members: [g.repos[j]],
        more: 0
      })
    }
    if (g.count > shown) {
      out.push({
        kind: "more",
        key: g.key + "#more",
        groupKey: g.key,
        groupLabel: "",
        groupCount: 0,
        label: (g.count - shown) + " more",
        reason: g.cause,
        action: g.action,
        count: g.count - shown,
        expanded: false,
        target: "",
        members: g.repos.slice(shown),
        more: g.count - shown
      })
    }
  }
  return out
}

// A transient deferral inside its retry budget is ONE quiet line, not a card:
// the slot is already handling it and there is nothing for anyone to do.
function waitingItems(repos) {
  var out = []
  var rs = repos || []
  for (var i = 0; i < rs.length; i++) {
    if (rs[i].attentionKind !== "transient") continue
    out.push({ repo: rs[i].name, reason: rs[i].attention ? String(rs[i].attention.reason || "") : "" })
  }
  return out
}

function attentionSinceText(item) {
  if (!item) return ""
  var bits = []
  if (item.runs > 0) bits.push("since " + item.runs + (item.runs === 1 ? " run" : " runs"))
  if (item.sinceAt !== "") bits.push(String(item.sinceAt).slice(0, 16).replace("T", " "))
  return bits.join(" · ")
}

// --------------------------------------------------------------- activity
//
// `shipwright activity --json` already did the counting; this only decides how
// dense a square looks. Four steps of ONE colour's alpha, never a hue ramp —
// the active theme is grayscale and a green heatmap would be the only coloured
// thing on the bar.
// Level 0 means NOTHING HAPPENED and is drawn as an empty cell, not as a pale
// filled one — the old ramp started at 0.16 alpha for zero and 0.38 for one, a
// difference nobody can see, so a blank fortnight and a quiet one looked alike.
//
// Levels 1-4 are relative to the busiest day IN THE WINDOW rather than to fixed
// thresholds. Fixed thresholds (3, 6) pin the top of the scale to somebody
// else's working day: at 20 commits a day every square saturates and the strip
// stops saying anything, and at 2 a day nothing ever leaves the first step.
// Scaling to the window's own peak spends the whole ramp on the range actually
// present, which is what makes the shape readable.
var ACTIVITY_STEPS = 4

function activityLevelFor(n, peak) {
  var v = toInt(n)
  if (v <= 0) return 0
  var p = toInt(peak)
  if (p <= 1) return ACTIVITY_STEPS
  var step = Math.ceil((v / p) * ACTIVITY_STEPS)
  return Math.max(1, Math.min(ACTIVITY_STEPS, step))
}

// Kept for the day grid, which has no window to scale against.
function activityLevel(commits) {
  var n = toInt(commits)
  if (n <= 0) return 0
  if (n < 3) return 1
  if (n < 6) return 2
  return 3
}

// What a cell counts: GitHub's number when there is one, the local count when
// GitHub could not be reached. One place, so the strip, the totals and the
// tooltip can never disagree about whether a day was empty.
//
// TODAY IS THE EXCEPTION, and it takes the larger of the two. A commit is not
// in GitHub's contributions calendar the instant the push returns — the API
// has its own propagation delay — while local git knew the moment it happened.
// Preferring GitHub unconditionally meant the square for the work you just did
// stayed a shade behind for minutes. Every other day is GitHub's number, which
// is what the profile page shows and what these squares are for.
function cellCount(cell) {
  if (!cell) return 0
  if (cell.isToday) return Math.max(toInt(cell.github), toInt(cell.commits))
  return cell.github > 0 ? cell.github : cell.commits
}

function parseActivity(raw) {
  var text = String(raw || "").trim()
  if (text === "") return { ok: false, lastError: "shipwright activity returned nothing" }
  var data
  try { data = JSON.parse(text) } catch (e) {
    return { ok: false, lastError: "could not parse shipwright activity output" }
  }
  if (!data || typeof data !== "object" || !Array.isArray(data.weeks)) {
    return { ok: false, lastError: "unexpected shipwright activity payload" }
  }
  return {
    ok: true,
    weeks: data.weeks,
    totals: data.totals || {},
    streak: data.streak || { current: 0, best: 0 },
    window: data.window || {},
    // Where the numbers came from, and how the two halves disagree. Both are
    // worded by the CLI so the bar and the terminal cannot phrase the same
    // fact differently.
    source: data.source || null,
    divergence: data.divergence || null,
    lastError: ""
  }
}

// Flattened to a single array of cells, because a Repeater over a grid is far
// simpler to reason about than nested ones — and the panel needs the day index
// anyway to draw the weekday labels.
function activityGrid(activity) {
  if (!activity || !Array.isArray(activity.weeks)) return []
  var today = activity.window ? String(activity.window.today || "") : ""
  var cells = []
  for (var w = 0; w < activity.weeks.length; w++) {
    var days = activity.weeks[w].days || []
    for (var d = 0; d < days.length; d++) {
      var day = days[d] || {}
      cells.push({
        week: w, day: d,
        date: String(day.date || ""),
        // `github` is what the square shows — it is the profile heatmap. When
        // GitHub is unavailable the CLI leaves it at 0 and `commits` carries
        // the local count, which is why `level` falls back rather than
        // rendering an empty year.
        github: toInt(day.github),
        note: String(day.note || ""),
        onDefault: toInt(day.on_default),
        offDefault: toInt(day.off_default),
        notPushed: toInt(day.not_pushed),
        commits: toInt(day.commits),
        byShipwright: toInt(day.by_shipwright),
        published: toInt(day.published),
        deferred: toInt(day.deferred),
        failed: toInt(day.failed),
        prs: toInt(day.prs),
        byRepo: day.by_repo || {},
        level: activityLevel(toInt(day.github) > 0 ? day.github : day.commits),
        isToday: today !== "" && String(day.date || "") === today
      })
    }
  }
  return cells
}

var WEEKDAY = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
var MONTH = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

function dayLabel(date) {
  var parts = String(date || "").split("-")
  if (parts.length !== 3) return String(date || "")
  var d = new Date(Number(parts[0]), Number(parts[1]) - 1, Number(parts[2]))
  if (isNaN(d.getTime())) return String(date || "")
  return WEEKDAY[d.getDay()] + " " + Number(parts[2]) + " " + MONTH[Number(parts[1]) - 1]
}

function dayTooltip(cell) {
  if (!cell) return ""
  var n = cellCount(cell)
  // "0 contributions" reads as a measurement that came back zero, which is the
  // ambiguity being complained about — it is not distinguishable from a day the
  // fetch never covered. Say the day was empty.
  var head = n <= 0
    ? dayLabel(cell.date) + " · nothing"
    : dayLabel(cell.date) + " · " + n + (n === 1 ? " contribution" : " contributions")
  var repos = []
  for (var name in cell.byRepo) repos.push(name + " " + cell.byRepo[name])
  var lines = [head]
  if (repos.length > 0) lines.push(repos.join(", "))
  // The CLI already worded why the two halves differ, and it names the CAUSE
  // — "on branches" is not "not pushed". Never re-derive it here.
  if (cell.note !== "" && cell.note !== head) lines.push(cell.note)
  // Say so when today is being drawn from local git because GitHub has not
  // counted it yet — otherwise the square disagrees with the profile page and
  // nothing explains why.
  if (cell.isToday && toInt(cell.commits) > toInt(cell.github))
    lines.push("counted here; github.com has not caught up yet")
  var runs = []
  if (cell.published > 0) runs.push(cell.published + " published")
  if (cell.deferred > 0) runs.push(cell.deferred + " deferred")
  if (cell.failed > 0) runs.push(cell.failed + " failed")
  if (cell.prs > 0) runs.push(cell.prs + " PR")
  if (runs.length > 0) lines.push(runs.join(" · "))
  return lines.join("\n")
}

// -------------------------------------------------------------- the slot
//
// How far through the gap between the last slot and the next one we are, 0..1.
// The rail is a clock, not a progress bar: nothing is "loading", the day is
// simply passing.
function slotProgress(run, nextSlot, nowMs) {
  if (!nextSlot || !nextSlot.at) return 0
  var next = Date.parse(nextSlot.at)
  if (!isFinite(next)) return 0
  var now = isFinite(nowMs) ? nowMs : Date.now()

  var started = run && run.started_at ? Date.parse(run.started_at) : NaN
  if (!isFinite(started) || started >= next) {
    // No last run to measure from: fall back to the interval the CLI reported.
    var span = toInt(nextSlot.in_s) * 1000
    if (span <= 0) return 1
    started = next - Math.max(span, 60000)
  }
  var span2 = next - started
  if (span2 <= 0) return 1
  return Math.max(0, Math.min(1, (now - started) / span2))
}

function humanDuration(seconds) {
  var s = Math.max(0, toInt(seconds))
  if (s < 60) return s + "s"
  var m = Math.floor(s / 60)
  if (m < 60) return m + "m"
  var h = Math.floor(m / 60)
  var rem = m % 60
  return rem === 0 ? h + "h" : h + "h" + (rem < 10 ? "0" : "") + rem + "m"
}

// The hero's detail pill. A run in progress outranks the countdown: what is
// happening now is more useful than what happens next.
function slotDetailText(run, nextSlot, running) {
  if (running && running.run_id) {
    var bits = ["running " + String(running.slot || "")]
    if (running.repo) bits.push(String(running.repo))
    if (running.stage) bits.push(String(running.stage))
    return bits.join(" · ")
  }
  if (!nextSlot || !nextSlot.slot) return ""
  return "next " + String(nextSlot.slot) + " · " + humanDuration(nextSlot.in_s)
}

// What the bar item says beside the glyph. Short enough for a bar, and silent
// when there is nothing to say.
function barLabelText(repos, attention) {
  var alerts = Array.isArray(attention) ? attention.length : 0
  if (alerts === 0) alerts = countByState(repos, "alert")
  if (alerts > 0) return alerts + " need you"
  var pending = countByState(repos, "pending")
  if (pending > 0) return pending + " pending"
  return ""
}

// ------------------------------------------------------- the activity strip
//
// The minimal view: ONE row of days, not the seven-row calendar. It answers
// "have I been working" at a glance and costs 27 px; the calendar answered
// "what did I do on the 14th", which is a question for `shipwright activity`
// in a terminal.
//
// 35 cells at 8 px with a 2 px gap is 348 of the 368 available — measured, not
// guessed. More days than that and the squares stop reading as separate days.
var STRIP_DAYS = 21

function stripCells(activity) {
  var all = activityGrid(activity)
  var cells = all.length > STRIP_DAYS ? all.slice(all.length - STRIP_DAYS) : all

  var peak = activitySaturation(cells)

  var out = []
  for (var j = 0; j < cells.length; j++) {
    var c = cells[j]
    var n = cellCount(c)
    var copy = {}
    for (var k in c) copy[k] = c[k]
    copy.count = n
    copy.empty = n <= 0
    copy.level = activityLevelFor(n, peak)
    out.push(copy)
  }
  return out
}

// ------------------------------------------------------- the calendar grid
//
// Seven weekday ROWS by N week COLUMNS — the shape of the GitHub profile graph,
// and the only one of the three tables drawn that shows a habit as a shape. A
// single row of days answers "have I been working"; this answers "when", which
// is the question a weekday actually carries. Two months of empty Fridays is
// invisible in a strip and unmissable here.
//
// Built from `activity.weeks` rather than from the flat day list, because the
// CLI already aligns weeks to Sunday and leaves the final one short at today.
// Re-deriving the columns from a flat list would mean re-deriving that boundary,
// and getting it wrong by one puts every weekday in the wrong row.
// How many week columns a month needs before it earns a label. Three is what
// keeps the labels evenly spaced: a month clipped to one or two columns by the
// edge of the window is drawn, but not named.
var MONTH_LABEL_MIN_COLS = 3

function calendarGrid(activity, wantWeeks) {
  if (!activity || !Array.isArray(activity.weeks)) return { rows: [], months: [], weeks: 0 }
  var all = activity.weeks
  var n = Math.max(1, Math.min(all.length, toInt(wantWeeks) || all.length))
  var cols = all.slice(all.length - n)
  var today = activity.window ? String(activity.window.today || "") : ""

  // The ramp saturates on what is DRAWN, so the darkest cell is the busiest day
  // in the window you are looking at rather than in a year you are not.
  var flat = []
  for (var a = 0; a < cols.length; a++) {
    var da = cols[a].days || []
    for (var b = 0; b < da.length; b++) flat.push(cellOf(da[b], today))
  }
  var peak = activitySaturation(flat)

  var rows = []
  for (var r = 0; r < 7; r++) {
    var row = []
    for (var c = 0; c < cols.length; c++) {
      var days = cols[c].days || []
      if (r >= days.length) { row.push(null); continue }
      var cell = cellOf(days[r], today)
      cell.level = activityLevelFor(cell.count, peak)
      row.push(cell)
    }
    rows.push(row)
  }

  // A month label sits over the first column that belongs to it. Repeating it on
  // every column is noise; omitting it entirely leaves 24 identical squares with
  // no way to say when any of them was.
  //
  // A COLUMN BELONGS TO THE MONTH OF ITS FIRST DAY, which is how GitHub assigns
  // them too: the week of 31 May to 6 June is May's, so June's label starts at
  // the column after it. Splitting a column between two months would put a
  // label over a boundary that is not where the eye sees the month change.
  //
  // AND A MONTH CLIPPED BY THE WINDOW EDGE GETS NO LABEL — this is the rule
  // that was missing, and it is why GitHub's leftmost columns are bare. A
  // 24-week window starting on 22 March gave March exactly two columns and
  // labelled it anyway, so "Mar" and "Apr" sat two columns apart while every
  // other pair sat four or five. Labels that are not evenly spaced stop reading
  // as a scale and start reading as a mistake, which is exactly what they were.
  // Dropping it costs nothing: the header already says how long the window is.
  var owner = []
  for (var m = 0; m < cols.length; m++) {
    var d0 = String((cols[m].days || [{}])[0].date || "")
    owner.push(d0 === "" ? "" : d0.slice(0, 7))   // YYYY-MM, unique in <= 53 weeks
  }
  var span = {}
  for (var s = 0; s < owner.length; s++)
    if (owner[s] !== "") span[owner[s]] = (span[owner[s]] || 0) + 1

  var months = []
  var seen = {}
  for (var t = 0; t < owner.length; t++) {
    var key = owner[t]
    if (key === "" || seen[key] === true || span[key] < MONTH_LABEL_MIN_COLS) {
      months.push("")
      continue
    }
    seen[key] = true
    months.push(MONTH[parseInt(key.slice(5, 7), 10) - 1])
  }

  return { rows: rows, months: months, weeks: cols.length, peak: peak }
}

// One day, in the shape the calendar and the strip both want.
function cellOf(day, today) {
  var d = day || {}
  var c = {
    date: String(d.date || ""),
    github: toInt(d.github),
    commits: toInt(d.commits),
    note: String(d.note || ""),
    onDefault: toInt(d.on_default),
    offDefault: toInt(d.off_default),
    notPushed: toInt(d.not_pushed),
    byShipwright: toInt(d.by_shipwright),
    published: toInt(d.published),
    deferred: toInt(d.deferred),
    failed: toInt(d.failed),
    prs: toInt(d.prs),
    byRepo: d.by_repo || {},
    isToday: today !== "" && String(d.date || "") === today
  }
  c.count = cellCount(c)
  c.empty = c.count <= 0
  c.level = 0
  return c
}

// Weekday labels down the left. Only alternate rows carry one, as GitHub does:
// seven 9 px labels stacked in 96 px is a smear, and Mon/Wed/Fri is enough to
// count from.
var WEEKDAY_RAIL = ["", "Mon", "", "Wed", "", "Fri", ""]

function calendarSummaryText(activity, wantWeeks) {
  if (!activity) return ""
  var g = calendarGrid(activity, wantWeeks)
  var total = 0, active = 0
  for (var r = 0; r < g.rows.length; r++)
    for (var c = 0; c < g.rows[r].length; c++) {
      var cell = g.rows[r][c]
      if (!cell) continue
      total += cell.count
      if (cell.count > 0) active++
    }
  var usingGithub = activity.source && activity.source.github && activity.source.github.ok
  return total + (usingGithub ? " contributions" : " commits") + " · " + active + " active"
}

function calendarHeaderText(weeks) {
  return "Last " + toInt(weeks) + " weeks"
}

// Where the ramp reaches full black. Not the maximum: one 43-commit day against
// a fortnight of 2s and 8s pushed every other square onto the first step, so the
// strip showed one dark cell and twenty identical pale ones. The saturation
// point is the 75th percentile of the days that HAPPENED — days above it are
// simply full, which is what "a big day" should look like anyway.
function activitySaturation(cells) {
  var hits = []
  for (var i = 0; i < cells.length; i++) {
    var n = cellCount(cells[i])
    if (n > 0) hits.push(n)
  }
  if (hits.length === 0) return 0
  hits.sort(function (a, b) { return a - b })
  var idx = Math.max(0, Math.ceil(hits.length * 0.75) - 1)
  return hits[idx]
}

// What the darkest square means, which on this grid is NOT "the busiest day".
//
// The ramp saturates at the 75th percentile of active days (activitySaturation),
// so the top quarter of them all paint at full strength — and the value moves
// when the window does, because it is computed over what is DRAWN. GitHub's
// legend can be a bare Less/More because its scale is fixed for the year; ours
// cannot, so the legend says the number the darkest step is reached at.
function calendarScaleText(peak) {
  var p = toInt(peak)
  if (p <= 0) return ""
  return p + "+ a day is darkest"
}

function stripHeaderText() {
  return "Last " + Math.round(STRIP_DAYS / 7) + " weeks"
}

// The right-hand side of the strip's caption row. Says which numbers these are,
// because "42 commits" and "42 contributions" are not the same claim.
function stripSummaryText(activity) {
  if (!activity) return ""
  var cells = stripCells(activity)
  var gh = 0, local = 0, days = 0
  for (var i = 0; i < cells.length; i++) {
    gh += cells[i].github
    local += cells[i].commits
    if ((cells[i].github > 0 ? cells[i].github : cells[i].commits) > 0) days++
  }
  var usingGithub = activity.source && activity.source.github && activity.source.github.ok
  var quiet = cells.length - days
  return (usingGithub ? gh : local) + (usingGithub ? " contributions" : " commits")
       + " · " + days + " active"
       + (quiet > 0 ? " · " + quiet + " quiet" : "")
}

// One line under the strip when GitHub is not the source, or is stale. Empty
// when everything is current — a caveat that is always on screen stops being
// read.
function activityCaveatText(activity) {
  if (!activity) return ""
  var src = activity.source && activity.source.github
  if (!src) return ""
  if (!src.ok) {
    return "from this machine — " + (src.error !== "" ? src.error : "GitHub unavailable")
  }
  var stale = toInt(src.stale_s)
  if (stale > 3600) return "github.com, as of " + humanDuration(stale) + " ago"
  return ""
}

// Which accounts the squares came from. Named, because a heatmap that silently
// counts two accounts is a heatmap you cannot reconcile with any one profile.
function activityAccountsText(activity) {
  if (!activity || !activity.source || !activity.source.github) return ""
  var a = activity.source.github.accounts || []
  var ok = []
  for (var i = 0; i < a.length; i++) if (a[i].ok && a[i].login) ok.push(a[i].login)
  return ok.join(", ")
}

// Today's number for the tile — GitHub's when it answered, local otherwise,
// the same rule the squares follow so the tile and the strip never disagree.
function todayCount(activity) {
  if (!activity || !activity.window) return 0
  var today = String(activity.window.today || "")
  var cells = activityGrid(activity)
  for (var i = 0; i < cells.length; i++) {
    if (cells[i].date !== today) continue
    return cells[i].github > 0 ? cells[i].github : cells[i].commits
  }
  return 0
}

// ---------------------------------------------------------------- the work tab

// The order the sections appear in, and it is not alphabetical: a batch already
// running outranks a decision you have not made, which outranks a comment
// somebody is waiting on. Anything not named here sorts last, so a new item
// kind from a future shipwright appears rather than vanishing.
var WORK_ORDER = ["agent", "run", "drift", "repo", "conflict", "check", "comment"]

// SENTENCE CASE, like every other band on this panel — "Needs you", "Always",
// "Pull requests", "Last 24 weeks". These went in lowercase and were the only
// headings on the widget that did, which read as a different kind of label
// rather than as the same one.
//
// `shipwright approve` is the exception and stays verbatim: it is a COMMAND,
// and the attention band already heads its groups with the command that clears
// them. Capitalising a command would make it a command you cannot paste.
var WORK_LABEL = {
  agent:    "Running now",
  run:      "Running now",
  drift:    "shipwright approve",
  repo:     "Ready to run",
  conflict: "Conflicts",
  check:    "Failed checks",
  comment:  "Review threads"
}

function workOrderOf(kind) {
  var i = WORK_ORDER.indexOf(String(kind))
  return i < 0 ? WORK_ORDER.length : i
}

// workRows(payload) — ONE FLAT LIST, with a groupLabel on the first row of each
// section.
//
// Flat because the cursor walks a single array and metrics() counts rowIndex
// uniqueness across it. A nested shape would put the cursor and metrics() out
// of step about how many rows exist, which is the exact class of bug the dup=
// counter was added to catch.
// workRepoRows(repos) — the repos you could run right now, from the health the
// widget already has. No extra process and no fetch: `uncommitted` and `ahead`
// are local git facts the fleet is already showing on the other tab.
//
// Only repos with something to do. Listing all ten would put a permanent
// ten-row section under a tab whose entire premise is that everything on it
// needs you.
function workRepoRows(repos) {
  var rs = Array.isArray(repos) ? repos : []
  var out = []
  for (var i = 0; i < rs.length; i++) {
    var r = rs[i]
    if (r.missing === true) continue
    var work = toInt(r.uncommittedN) + toInt(r.aheadN)
    if (work <= 0 && r.paused !== true) continue
    var bits = []
    if (toInt(r.uncommittedN) > 0) bits.push(r.uncommittedN + " uncommitted")
    if (toInt(r.aheadN) > 0) bits.push(r.aheadN + " unpushed")
    if (r.paused === true) bits.push("paused")
    else if (r.armed !== true) bits.push("dry run only")
    out.push({
      kind: "repo",
      id: "repo:" + String(r.name),
      repo: String(r.name),
      title: bits.join(" \u00b7 "),
      paused: r.paused === true,
      action: "shipwright now " + String(r.name)
    })
  }
  return out
}

function workRows(payload, repos) {
  var p = payload && typeof payload === "object" ? payload : {}
  var running = Array.isArray(p.running) ? p.running : []
  var waiting = Array.isArray(p.waiting) ? p.waiting : []

  var all = running.concat(waiting).concat(workRepoRows(repos))

  // GROUPED BY KIND *AND* PULL REQUEST, not by kind alone.
  //
  // Fifteen review threads on one pull request are one piece of work, and the
  // agent lane already treats them that way: `shipwright agent --pr N` takes the
  // whole batch. Grouping only by kind meant the heading could not carry a
  // button, so acting on fifteen threads meant fifteen clicks that each started
  // a separate batch — the opposite of what the lane is for.
  var buckets = {}
  var order = []
  for (var i = 0; i < all.length; i++) {
    var k = String(all[i].kind || "other")
    // agent and run share one heading: from the outside they are both "work
    // that is happening without you", and splitting them would put a one-row
    // section above a one-row section.
    var section = (k === "agent" || k === "run") ? "running" : k
    var pr = all[i].pr !== undefined && all[i].pr !== null ? String(all[i].pr) : ""
    // Only the blocked kinds split by pull request. A drifted contract and a
    // repo waiting to run have no PR, and a running batch is already one row.
    var key = (section === "running" || pr === "")
      ? section
      : section + "\u0000" + String(all[i].repo || "") + "\u0000" + pr
    if (!buckets[key]) { buckets[key] = []; order.push(key) }
    buckets[key].push(all[i])
  }

  // Stable within a group: the payload's own order is meaningful (newest batch
  // first, the inbox in the order the sweep found it), so only the SECTION is
  // sorted, never the rows inside one.
  order.sort(function (a, b) {
    var ka = String(a).split("\u0000")[0], kb = String(b).split("\u0000")[0]
    ka = ka === "running" ? "agent" : ka
    kb = kb === "running" ? "agent" : kb
    var d = workOrderOf(ka) - workOrderOf(kb)
    return d !== 0 ? d : (a < b ? -1 : a > b ? 1 : 0)
  })

  var out = []
  for (var s = 0; s < order.length; s++) {
    var rows = buckets[order[s]]
    for (var j = 0; j < rows.length; j++) {
      var r = rows[j]
      var kind = String(r.kind || "other")
      var isAgent = kind === "agent"
      var total = Number(r.total || 0)
      var done = Number(r.done || 0)
      var pr = r.pr !== undefined && r.pr !== null ? String(r.pr) : ""
      out.push({
        kind: kind,
        key: String(r.id || (kind + "#" + s + "#" + j)),
        // The heading is drawn by the FIRST row of its section, the same way
        // the attention band does it, so a heading can never outlive its rows.
        groupLabel: j === 0
          ? (String(order[s]).indexOf("\u0000") >= 0 || order[s] === "running"
             ? (order[s] === "running" ? WORK_LABEL.agent : (WORK_LABEL[kind] || kind))
             : (WORK_LABEL[kind] || kind))
          : "",
        // Which pull request the whole group belongs to, so the heading can say
        // what its buttons will act on.
        groupSub: (j === 0 && pr !== "") ? (String(r.repo || "") + " #" + pr) : "",
        // THE WHOLE STACK IN ONE PRESS. `shipwright agent --pr N` is already a
        // batch command; without a button on the heading the only way to reach
        // it from the bar was to click one row, which starts a batch for that
        // pull request anyway — so fifteen clicks did fifteen times the same
        // thing. Only groups that share a pull request get them.
        groupButtons: (j === 0 && pr !== "" && kind !== "agent" && kind !== "run")
          ? ["agent", "read"] : [],
        groupPr: pr,
        groupRepo: String(r.repo || ""),
        groupCount: j === 0 ? rows.length : 0,
        label: workLabelFor(r),
        detail: String(r.title || r.label || ""),
        repo: String(r.repo || ""),
        pr: r.pr !== undefined && r.pr !== null ? String(r.pr) : "",
        state: String(r.state || ""),
        action: String(r.action || ""),
        url: String(r.url || ""),
        planPath: String(r.plan_path || ""),
        // 0..1, and only for a batch that knows its own size. A rail bound to
        // 0/0 draws a full bar for work that has not started.
        // 0..1 for anything that knows its own size — an agent batch counts
        // threads, a run counts repos. Still gated on total: a rail bound to
        // 0/0 would draw a full bar for work that has not started.
        progress: total > 0 ? (done / total) : -1,
        done: done,
        total: total,
        holdUntil: Number(r.hold_until || 0),
        buttons: workButtonsFor(r)
      })
    }
  }
  return out
}

function workLabelFor(r) {
  var repo = String(r.repo || "")
  var pr = r.pr !== undefined && r.pr !== null ? (" #" + r.pr) : ""
  return repo + pr
}

// The buttons a row carries, and every one of them does the obvious thing.
// A row with no button is a row that only reports, which is what the Fleet tab
// is for — this tab exists because those rows had nothing to press.
function workButtonsFor(r) {
  var kind = String(r.kind || "")
  var state = String(r.state || "")
  if (kind === "agent") {
    if (state === "holding") return ["go", "stop"]
    if (state === "running") return ["pause", "stop"]
    if (state === "paused")  return ["go", "stop"]
    return []
  }
  if (kind === "run")   return []
  if (kind === "drift") return ["approve", "read"]
  // A paused repo cannot run at all, so the only useful button is the one that
  // un-pauses it — offering `run` there would be a button that does nothing.
  if (kind === "repo")  return r.paused === true ? ["resume"] : ["run", "dry"]
  return ["agent", "read"]
}

// The count the tab's chip shows: things WAITING ON YOU, not things happening.
// A running batch is not a number you need on a chip; it is already visible on
// the tab, and counting it would make the chip go up when work starts.
function workWaitingCount(payload) {
  var p = payload && typeof payload === "object" ? payload : {}
  return Array.isArray(p.waiting) ? p.waiting.length : 0
}
