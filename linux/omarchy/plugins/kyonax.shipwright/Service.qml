import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import "Model.js" as Model

// Headless half of the shipwright bar plugin: owns the CLI, the refresh cadence
// and the parsed state. The Panel renders whatever lands here and decides
// nothing on its own.
//
// Two refresh paths, because `shipwright health` has two costs:
//
//   refresh()      full check, fetches every repo's remote. Expensive (seconds),
//                  so it is on a long timer and on explicit user request.
//   refreshQuick() `--no-fetch`, local state only. Cheap, so it runs every time
//                  the popup opens and the panel never shows a stale table.
//
// The two paths get their OWN Process, and that is the whole point rather than
// an implementation detail. Sharing one meant a single `if (running) return`
// covering both, so opening the popup while the slow check was in flight
// silently did nothing and showed the very table the click was meant to
// replace. Measured on this fleet: the full check takes ~26s and the quick one
// ~0.5s, and the full one is on a `triggeredOnStart` timer — so a click was
// dropped for ~26s out of every 300, and ALWAYS for the first 26s after login,
// which is exactly when someone looks at the bar.
//
// `--no-fetch` touches no remote, so running it alongside a fetch is safe.
Item {
  id: root

  property var settings: ({})

  readonly property string home: Quickshell.env("HOME")
  readonly property string shipwrightBin: home + "/.local/bin/shipwright"

  property bool available: true
  property var run: null
  property var repos: []
  property var notes: []
  property var prs: []
  property int prsMine: 0
  property var attention: []
  property var nextSlot: null
  property var running: null
  property string lastError: ""
  property bool loaded: false

  // The activity payload is a THIRD thing with a third cost. It reads ten
  // working trees' git logs, so it is neither the 15s poll nor free — it is
  // fetched when the Activity page is first opened and refreshed after a full
  // check lands, and the CLI caches it for ten minutes underneath.
  property var activity: null
  property bool activityLoaded: false
  property string activityError: ""
  property int activityWeeks: 12
  property int activitySeq: 0
  property int activityAppliedSeq: 0
  // The Work tab. ONE payload for four things — agent batches, the inbox,
  // contract drift and the live run — so the tab is one read rather than four
  // Processes racing to render half a picture each.
  property var work: null
  property bool workLoaded: false
  property string workError: ""
  property int workSeq: 0
  property int workAppliedSeq: 0
  readonly property bool workRunning: workProcess.running
  readonly property bool activityRunning: activityProcess.running

  // Results are applied in the order they were ASKED FOR, never the order they
  // happen to come back in. Without this, a full check that started before you
  // made a commit lands ~26s later and overwrites the quick check that saw it —
  // the panel would go backwards in front of you.
  property int seq: 0
  property int appliedSeq: 0
  property int fullQueued: 0
  property double lastFullAt: 0

  // `busy` is the EXPENSIVE check only, because that is what the panel greys
  // its re-check button on. A half-second local re-read is not worth disabling
  // a button for — and greying it out during the slow check was the one moment
  // the owner most wanted to press it.
  readonly property bool quickRunning: quickProcess.running
  readonly property bool busy: healthProcess.running
  readonly property bool refreshing: healthProcess.running || quickProcess.running

  // How stale the fetched columns may be before opening the popup pays for a
  // new fetch. `behind` cannot be known without one, so a click has to be
  // allowed to buy that occasionally, without buying it on every glance.
  readonly property int staleAfterSec: Math.min(60, refreshIntervalSec)
  readonly property int refreshIntervalSec: intSetting("refreshIntervalSec", 300, 30, 3600)
  readonly property string fleetState: Model.fleetState(repos)
  readonly property var tierGroups: Model.tierGroups(repos)
  readonly property var attentionItems: Model.attentionItems(attention, repos)
  readonly property var waitingItems: Model.waitingItems(repos)
  readonly property int attentionCount: attentionItems.length
  readonly property string slotDetailText: Model.slotDetailText(run, nextSlot, running)
  readonly property string barLabelText: Model.barLabelText(repos, attentionItems)

  // How long ago the fleet was last actually checked against its remotes. Bound
  // to `tick` so it re-evaluates while the popup is open — a readonly property
  // over Date.now() alone would freeze at whatever it said when it was built.
  property int tick: 0
  // Deliberately terse, and it got terser when the third tab arrived. It has
  // exactly one consumer: the slack the chips leave beside them. Two chips left
  // room for "checked 19s ago"; three leave about eighty pixels, so the old
  // wording elided on EVERY frame rather than in the worst case the layout was
  // designed around. Beside a row of tabs, "19s ago" says the same thing.
  readonly property string lastCheckedText: {
    tick
    if (lastFullAt <= 0) return busy ? "checking…" : "not checked"
    if (busy) return "checking…"
    var secs = Math.max(0, Math.floor(nowSec() - lastFullAt))
    return Model.humanDuration(secs) + " ago"
  }
  readonly property int alertCount: Model.countByState(repos, "alert")
  readonly property int pendingCount: Model.countByState(repos, "pending")
  readonly property string runText: Model.runText(run)
  readonly property string runSlotText: Model.runSlotText(run)
  readonly property string fleetSummaryText: Model.fleetSummaryText(repos)
  readonly property string prsSummaryText: Model.prsSummaryText(prs, prsMine)

  // The PR tab's filter. `mine` means shipwright raised it, which is the case
  // that matters: restore_branch already put the checkout back, so that work is
  // on its branch and NOT in the working tree until it merges.
  property bool prsMineOnly: false
  readonly property var prsVisible: prsMineOnly
    ? prs.filter(function (p) { return p.mine })
    : prs

  signal refreshed()

  function setting(name, fallback) {
    var value = settings ? settings[name] : undefined
    return value === undefined || value === null ? fallback : value
  }

  function intSetting(name, fallback, min, max) {
    var n = parseInt(String(setting(name, fallback)), 10)
    if (!isFinite(n)) n = fallback
    if (n < min) n = min
    if (n > max) n = max
    return n
  }

  // What the popup calls every time it opens. It must NEVER be a no-op: the
  // panel's whole promise is that what you are looking at is what the click
  // revealed.
  //
  // Both halves, deliberately. The local re-read is instant and always runs, so
  // the table is right the moment it appears; the fetch is started only when
  // the remote-derived columns have gone stale, so glancing at the bar twice in
  // a row does not queue two minutes of git.
  function refreshOnOpen() {
    refreshQuick()
    if (!healthProcess.running && (nowSec() - lastFullAt) >= staleAfterSec) refresh()
  }

  function nowSec() { return Date.now() / 1000 }

  // The full check. An explicit ask is never dropped: if one is already in
  // flight the request is REMEMBERED and honoured when that one lands, because
  // the owner pressing re-check and nothing happening is the complaint this
  // whole file exists to answer.
  function refresh() {
    if (healthProcess.running) { fullQueued = 1; return }
    fullQueued = 0
    seq += 1
    healthProcess.seq = seq
    healthProcess.command = [shipwrightBin, "health", "--json"]
    healthProcess.running = true
  }

  // The cheap check, on its own process so the slow one can never block it.
  function refreshQuick() {
    if (quickProcess.running) return
    seq += 1
    quickProcess.seq = seq
    quickProcess.command = [shipwrightBin, "health", "--no-fetch", "--json"]
    quickProcess.running = true
  }

  function applyHealth(raw, forSeq) {
    // Anything older than what is already on screen is dropped on the floor.
    if (forSeq < appliedSeq) return

    var parsed = Model.parseHealth(raw)

    // A read that came back empty or unparsable is a FAILED MEASUREMENT, not a
    // fleet with no repos in it. It reports itself and changes nothing else.
    //
    // Crucially it does NOT claim the sequence. Claiming it meant a fast
    // failure could overtake a slower good result — the quick local read and
    // the full remote one run at the same time now — and that good payload was
    // then dropped as stale, leaving the panel showing an error it already had
    // the data to replace. A process killed mid-read (a shell restart, a
    // logout) produces exactly this.
    if (!parsed.ok) {
      lastError = parsed.lastError
      loaded = true
      return
    }

    appliedSeq = forSeq

    run = parsed.run
    repos = parsed.repos
    notes = parsed.notes
    prs = parsed.prs || []
    prsMine = parsed.prsMine || 0
    attention = parsed.attention || []
    nextSlot = parsed.nextSlot || null
    running = parsed.running || null
    lastError = ""
    loaded = true
    root.refreshed()
  }

  // The activity grid. Its own process and its own sequence counter, for
  // exactly the reason the other two have theirs: a check that takes seconds
  // must never be able to swallow a click, and a result that started before
  // your last commit must never overwrite one that saw it.
  // ALWAYS --no-refresh. The heatmap's numbers come from GitHub now, and this
  // process runs on the popup's open path — the one thing the whole two-process
  // rework existed to keep instant. `--no-refresh` skips the fetch before any
  // age check, so this call is a disk read whatever the cache's age or the
  // network's mood. The CLI has a test that replaces `gh` with a script exiting
  // 99 to prove it.
  //
  // `force` still recomputes the derived payload (--no-cache); it does not and
  // must not reach GitHub.
  function refreshActivity(force) {
    if (activityProcess.running) return
    activitySeq += 1
    activityProcess.seq = activitySeq
    var args = [shipwrightBin, "activity", "--json", "--no-refresh",
                "--weeks", String(activityWeeks)]
    if (force === true) args.push("--no-cache")
    activityProcess.command = args
    activityProcess.running = true
  }

  // The one path that MAY spend an API call, and only because a person asked:
  // the re-check button, which already costs ~26s and is never on a timer.
  // Detached so a slow GitHub cannot hold the panel open.
  function syncActivity() {
    Quickshell.execDetached([shipwrightBin, "activity", "--json", "--refresh"])
  }

  function setActivityWeeks(weeks) {
    var w = Math.max(1, Math.min(53, parseInt(String(weeks), 10) || 12))
    if (w === activityWeeks) return
    activityWeeks = w
    activityLoaded = false
    refreshActivity(false)
  }

  function applyActivity(raw, forSeq) {
    if (forSeq < activityAppliedSeq) return
    activityAppliedSeq = forSeq
    var parsed = Model.parseActivity(raw)
    if (!parsed.ok) { activityError = parsed.lastError; activityLoaded = true; return }
    activity = parsed
    activityError = ""
    activityLoaded = true
  }

  function applyWork(raw, forSeq) {
    // The same seq guard the other three carry. Two refreshes in flight and the
    // slower one landing last would show older state than the tab already had.
    if (forSeq < workAppliedSeq) return
    workAppliedSeq = forSeq
    try {
      work = JSON.parse(raw)
      workError = ""
    } catch (e) {
      // Keep the last good payload. An empty Work tab reads as "nothing is
      // waiting on you", which is the most misleading thing it could say.
      workError = "could not read shipwright work"
    }
    workLoaded = true
  }

  function refreshWork() {
    if (workProcess.running) return
    workSeq += 1
    workProcess.seq = workSeq
    // --no-refresh, always. The bar never fetches: this reads the caches a run
    // or the watch timer wrote, and returns before any age check.
    workProcess.command = [shipwrightBin, "work", "--json", "--no-refresh"]
    workProcess.running = true
  }

  // The buttons on a Work row. Detached, for the same reason launchTui is: a
  // Process is a child of the shell, so re-assigning `command` while one is
  // still running silently drops the second press — which on a row of buttons
  // is the difference between "stop" working and appearing to.
  function workAction(cmd, arg) {
    if (!cmd) return
    var args = [shipwrightBin]
    var parts = String(cmd).split(" ")
    for (var i = 0; i < parts.length; i++) if (parts[i] !== "") args.push(parts[i])
    if (arg !== undefined && arg !== null && String(arg) !== "") args.push(String(arg))
    Quickshell.execDetached(args)
    // Ask again shortly: the state file has changed, and a button that does not
    // visibly do anything is one people press twice.
    workRecheck.restart()
  }

  Timer {
    id: workRecheck
    interval: 700
    repeat: false
    onTriggered: root.refreshWork()
  }

  // `shipwright logs --list` and friends are TUIs; the panel launches them
  // through omarchy so they land in a styled terminal instead of nowhere.
  //
  // execDetached, not a Process. A Process is a CHILD of the shell: it is
  // killed when the shell reloads, its stdio is plumbed back here for no
  // reason, and re-assigning `command` while one is still running silently
  // drops the second launch — the same shared-object bug that made the popup
  // ignore clicks. A terminal the operator opens should outlive the widget
  // that opened it. tempo-hours documents the same fix.
  // THE APP ID IS A FLAG, NOT A POSITIONAL. omarchy-launch-or-focus-tui's
  // signature is `[--app-id=<id>] <command> [args...]`, so passing the id bare
  // makes it the COMMAND: every call here died with
  //   Failed to spawn command 'shipwright-inbox': No such file or directory
  // detached, where nothing shows it. That silently broke `read`, the ledger,
  // `L` logs, `H` health and the middle-click for as long as it has been wrong.
  function launchTui(appId, command) {
    Quickshell.execDetached(["omarchy-launch-or-focus-tui", "--app-id=" + appId, command])
  }

  // A PR row is a link. xdg-open rather than a hardcoded browser, because the
  // run stage opens PRs the same way and the two must not disagree about which
  // browser the owner uses.
  function openUrl(url) {
    if (!url) return
    Quickshell.execDetached(["xdg-open", String(url)])
  }

  // Open a repository where the work actually gets done.
  //
  // Through the CLI, NOT omarchy-launch-editor directly. That launcher runs
  // `setsid uwsm-app -- emacs <path>` for a graphical editor, which starts a
  // SECOND Emacs while the owner is sitting in a Doom daemon that has been up
  // for days — the file opens tens of seconds later in a window nobody is
  // looking at. `shipwright edit` resolves the running session first and falls
  // back to the launcher, and putting it there rather than here means the
  // terminal and the bar cannot disagree about what opening a repo means.
  //
  // argv as an ARRAY, never a joined string: every repo path here contains a
  // space ("…/Da_ Disk/…"), and a shell-quoted command string would open two
  // wrong directories instead of one right one.
  function openEditor(path) {
    if (!path) return
    Quickshell.execDetached([shipwrightBin, "edit", String(path)])
  }

  // WHY a repo did not ship: the standing, the files uncommitted right now, the
  // gate that failed with the tail of its log, what the dossier saw, and the
  // last eight runs. The CLI writes it and opens it; the bar only asks.
  //
  // IT ALSO TAKES AN AGENT TASK ID (`agent-...`), and answers the same question
  // for a batch that was refused: what was refused, the reason in the planner's
  // own words, how to solve it and what to run. One verb, because it is one
  // question — the Work tab's `why` pill passes a task id here unchanged.
  //
  // This is the popup's answer to "and then what?". A status line can say
  // `skipped-drift, 5 runs`; it cannot say that the five runs before those were
  // all deferred-hot because the repo is edited every ten minutes, which is the
  // thing you would actually want to know.
  function openReport(repo) {
    if (!repo) return
    Quickshell.execDetached([shipwrightBin, "why", String(repo)])
  }

  // The pull request template this repo uses — or an empty buffer at the path
  // GitHub reads first, when it has none.
  function openTemplate(repo) {
    if (!repo) return
    Quickshell.execDetached([shipwrightBin, "template", String(repo), "--show"])
  }

  // The report, plus a model's reading of why it keeps happening. SPENDS MONEY
  // (~$0.20), so it is bound to its own key and never to a click that something
  // else already means.
  function advise(repo) {
    if (!repo) return
    Quickshell.execDetached([shipwrightBin, "advise", String(repo)])
  }

  // Open every PR at once — the morning counterpart to the browser rule, which
  // deliberately declines to do this at 03:00.
  function openAllPrs() {
    if (!prs || prs.length === 0) return
    Quickshell.execDetached([shipwrightBin, "prs", "--open"])
  }

  // The attention page's one action. Optimistic, the way dropbox's is: the row
  // says "running…" the moment it is pressed, because the run takes seconds and
  // a button that looks unpressed for that long gets pressed twice.
  property var runningNow: ({})
  // runNow(repo, dry) — the full pipeline against one repo, right now.
  //
  // A TERMINAL, not a detached process. This is the one action on the bar that
  // can PUBLISH, it takes tens of seconds, and it prints why it deferred; a
  // silent background launch would leave the operator watching a widget that
  // says nothing until it is over. `dry` runs the identical pipeline and stops
  // before anything is written, which is what makes it safe to offer beside it.
  function runNow(repo, dry) {
    if (!repo) return
    var next = {}
    for (var k in runningNow) next[k] = runningNow[k]
    next[repo] = true
    runningNow = next
    var cmd = shipwrightBin + " now " + repo + (dry === true ? " --dry-run" : "")
    Quickshell.execDetached(["omarchy-launch-or-focus-tui", "--app-id=shipwright-now", cmd])
    // A run changes the queue, the ledger and the inbox; ask again once it has
    // had a moment, so the tab is not left describing the state before it.
    workRecheck.restart()
  }
  function clearRunningNow() { runningNow = ({}) }

  // The slow one: fetches every repo's remote. Seconds, not milliseconds.
  Process {
    id: healthProcess
    property int seq: 0
    stdout: StdioCollector {
      waitForEnd: true
      onStreamFinished: root.applyHealth(text, healthProcess.seq)
    }
    onExited: function(exitCode) {
      // 127 is the shell's "no such command"; anything else non-zero still
      // produced no parseable payload, so say so rather than showing a
      // table that silently stopped updating.
      if (exitCode === 127) {
        root.available = false
        root.lastError = "shipwright is not installed"
        root.loaded = true
      } else if (exitCode === 0) {
        root.lastFullAt = root.nowSec()
        // The fetch has just updated this machine's remote-tracking refs on
        // disk, so a local re-read now sees BOTH the fresh remote state and
        // whatever changed in the working tree during the ~26s it took.
        //
        // It also settles the ordering rule honestly. A click during the fetch
        // produces a newer result, which correctly wins — but that would
        // otherwise throw the fetch away and leave `behind` stale until the
        // next cycle. Re-reading costs half a second and means the fetch is
        // never wasted, whether or not anyone clicked while it ran.
        root.refreshQuick()
        // The runs that just finished are part of the picture the grid draws,
        // and the CLI's own cache means this is usually free.
        //
        // `activityLoaded` is the wrong gate on the one path that matters. The
        // first check after login has not loaded the grid yet, so a run that
        // published during it left the calendar showing the day before — and
        // the CLI drops its derived cache precisely when a run published, so
        // this is the read that picks the new numbers up.
        root.refreshActivity(true)
        root.clearRunningNow()
      }
      // An ask that arrived while this one was running is honoured now rather
      // than forgotten.
      if (root.fullQueued === 1 && root.available) {
        root.fullQueued = 0
        root.refresh()
      }
    }
  }

  // The fast one: local state only, so it can run while the slow one fetches.
  Process {
    id: quickProcess
    property int seq: 0
    stdout: StdioCollector {
      waitForEnd: true
      onStreamFinished: root.applyHealth(text, quickProcess.seq)
    }
    onExited: function(exitCode) {
      if (exitCode === 127) {
        root.available = false
        root.lastError = "shipwright is not installed"
        root.loaded = true
      }
    }
  }

  // The activity check. A third Process, not a third caller of an existing one:
  // sharing would put the same `if (running) return` over two operations with
  // different costs, which is the defect this plugin has already been bitten by
  // twice.
  Process {
    id: activityProcess
    property int seq: 0
    stdout: StdioCollector {
      waitForEnd: true
      onStreamFinished: root.applyActivity(text, activityProcess.seq)
    }
    onExited: function(exitCode) {
      if (exitCode === 127) {
        root.activityError = "shipwright is not installed"
        root.activityLoaded = true
      } else if (exitCode !== 0 && !root.activityLoaded) {
        // An older shipwright has no `activity` command at all. Say that once,
        // rather than leaving an empty grid that reads as "you did nothing".
        root.activityError = "this shipwright has no 'activity' command yet"
        root.activityLoaded = true
      }
    }
  }

  // A FOURTH Process, never a fourth caller of an existing one. The header of
  // this file records what sharing one cost: clicks dropped for 26 seconds at a
  // time, because `if (running) return` was guarding two operations with
  // completely different costs.
  Process {
    id: workProcess
    property int seq: 0
    stdout: StdioCollector {
      waitForEnd: true
      onStreamFinished: root.applyWork(text, workProcess.seq)
    }
    onExited: function(exitCode) {
      if (exitCode === 127) {
        root.workError = "shipwright is not installed"
        root.workLoaded = true
      } else if (exitCode !== 0 && !root.workLoaded) {
        // An older shipwright has no `work` command. Say so once, rather than
        // leaving an empty tab that reads as "nothing is waiting on you".
        root.workError = "this shipwright has no 'work' command yet"
        root.workLoaded = true
      }
    }
  }

  Timer {
    id: fullRefreshTimer
    interval: root.refreshIntervalSec * 1000
    running: root.available
    repeat: true
    triggeredOnStart: true
    onTriggered: root.refresh()
  }

  // The only thing that ran at startup was fullRefreshTimer's `triggeredOnStart`
  // — the SLOW check, which talks to every remote and takes the better part of
  // half a minute. Until it landed the widget held nothing at all, so the first
  // click opened an empty panel and then waited another second and a half for
  // the local read that opening triggers.
  //
  // Kick the cheap read the moment the widget loads. The panel has a complete
  // local table about a second in and never shows a blank one again; the slow
  // check replaces it when it arrives. Which of the two finishes first does not
  // matter — applyHealth drops any payload older than what is already on screen.
  Component.onCompleted: root.refreshQuick()
}
