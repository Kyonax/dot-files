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
  property string lastError: ""
  property bool loaded: false

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
  readonly property int alertCount: Model.countByState(repos, "alert")
  readonly property int pendingCount: Model.countByState(repos, "pending")
  readonly property string runText: Model.runText(run)
  readonly property string runSlotText: Model.runSlotText(run)
  readonly property string fleetSummaryText: Model.fleetSummaryText(repos)
  readonly property string prsSummaryText: Model.prsSummaryText(prs, prsMine)

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
    appliedSeq = forSeq

    var parsed = Model.parseHealth(raw)

    if (!parsed.ok) {
      lastError = parsed.lastError
      loaded = true
      return
    }

    run = parsed.run
    repos = parsed.repos
    notes = parsed.notes
    prs = parsed.prs || []
    prsMine = parsed.prsMine || 0
    lastError = ""
    loaded = true
    root.refreshed()
  }

  // `shipwright logs --list` and friends are TUIs; the panel launches them
  // through omarchy so they land in a styled terminal instead of nowhere.
  function launchTui(appId, command) {
    tuiProcess.command = ["omarchy-launch-or-focus-tui", appId, command]
    tuiProcess.running = true
  }

  // A PR row is a link. xdg-open rather than a hardcoded browser, because the
  // run stage opens PRs the same way and the two must not disagree about which
  // browser the owner uses.
  function openUrl(url) {
    if (!url) return
    openProcess.command = ["xdg-open", String(url)]
    openProcess.running = true
  }

  // Open every PR at once — the morning counterpart to the browser rule, which
  // deliberately declines to do this at 03:00.
  function openAllPrs() {
    if (!prs || prs.length === 0) return
    tuiProcess.command = [shipwrightBin, "prs", "--open"]
    tuiProcess.running = true
  }

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

  Process {
    id: tuiProcess
  }

  Process {
    id: openProcess
  }

  Timer {
    id: fullRefreshTimer
    interval: root.refreshIntervalSec * 1000
    running: root.available
    repeat: true
    triggeredOnStart: true
    onTriggered: root.refresh()
  }
}
