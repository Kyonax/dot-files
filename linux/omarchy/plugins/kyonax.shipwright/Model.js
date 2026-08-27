// Pure display math for the shipwright bar plugin. Qt-free on purpose, the way
// omarchy's own KeyboardLayoutModel.js and dropbox/Model.js are, so every rule
// here can be reasoned about (and tested) without standing up a shell.
//
// `shipwright health --json` is the single source of truth. Nothing in this
// file invents state; it only decides how the state should read.

.pragma library

// ---------------------------------------------------------------- parsing

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

  var repos = Array.isArray(data.repos) ? data.repos.map(normalizeRepo) : []
  var notes = Array.isArray(data.notes) ? data.notes.map(String) : []
  var prs = Array.isArray(data.prs) ? data.prs.map(normalizePr) : []

  return {
    ok: true,
    run: data.run || null,
    repos: repos,
    notes: notes,
    prs: prs,
    prsMine: toInt(data.prs_mine),
    lastError: ""
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
    branch: String(repo.branch || ""),
    ahead: String(repo.ahead || "0"),
    behind: String(repo.behind || "0"),
    uncommitted: String(repo.uncommitted || "0"),
    aheadN: toInt(repo.ahead),
    behindN: toInt(repo.behind),
    uncommittedN: toInt(repo.uncommitted),
    outcome: String(repo.outcome || ""),
    missing: repo.missing === true,
    reason: String(repo.reason || "")
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
  var outcome = String(repo.outcome || "").toLowerCase()
  if (outcome === "vetoed" || outcome === "failed" || outcome === "error") return "alert"
  if (repo.reason !== "") return "alert"
  if (repo.aheadN > 0 || repo.behindN > 0 || repo.uncommittedN > 0) return "pending"
  return "settled"
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

// The full reason belongs in a tooltip, not the row -- it can be a sentence.
function repoTooltip(repo) {
  if (!repo) return ""
  if (repo.reason !== "") return repo.reason
  if (repo.missing) return repo.name + ": repo path does not exist (disk unmounted?)"
  return repo.name + ": " + repoDetailText(repo)
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
