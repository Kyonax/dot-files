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
// A single `busy` guard replaces the flock the bash module needed: Process
// exposes `running`, so an overlapping refresh simply never starts.
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
  property bool refreshing: false

  readonly property bool busy: healthProcess.running
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

  function refresh() { start(false) }
  function refreshQuick() { start(true) }

  function start(noFetch) {
    if (healthProcess.running) return
    refreshing = true
    healthProcess.command = noFetch
      ? [shipwrightBin, "health", "--no-fetch", "--json"]
      : [shipwrightBin, "health", "--json"]
    healthProcess.running = true
  }

  function applyHealth(raw) {
    var parsed = Model.parseHealth(raw)
    refreshing = false

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

  Process {
    id: healthProcess
    stdout: StdioCollector {
      waitForEnd: true
      onStreamFinished: root.applyHealth(text)
    }
    onExited: function(exitCode) {
      root.refreshing = false
      // 127 is the shell's "no such command"; anything else non-zero still
      // produced no parseable payload, so say so rather than showing a
      // table that silently stopped updating.
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
