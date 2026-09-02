import QtQuick
import Quickshell
import Quickshell.Io

// Service.qml — drives the CLI and holds the parsed result.
//
// TWO COSTS, TWO CADENCES
//   refresh()       full check; asks Tempo what is logged. Seconds, and a
//                   network round trip, so it runs on a long timer and on
//                   explicit request only.
//   refreshQuick()  --no-fetch: local state, no network at all. Cheap enough to
//                   run every time the popup opens, so the panel is never stale.
//
// EACH COST GETS ITS OWN Process, and that is load-bearing rather than tidy.
// Sharing one meant a single `if (running) return` covering both, and the panel
// opens by calling refreshQuick() and then Qt.callLater(refresh()) — so the
// quick read set `running` and the fetch that was supposed to follow it was
// silently discarded EVERY TIME the popup opened. The bar showed cached hours
// and never went to Tempo, which is the opposite of what those two lines say.
//
// Measured: `status --json` ~2.0s (network), `status --no-fetch --json` ~0.02s
// (replays the cache). Two processes, and the anti-stampede guard stays — one
// per path instead of one for both.

import "Model.js" as Model

QtObject {
    id: root

    property string bin: "tempo-hours"
    property int refreshIntervalSec: 900

    property var snap: Model.parse("")
    property bool loaded: false
    property string lastError: ""
    property bool available: true

    // Results are applied in the order they were ASKED for. Without it the 2s
    // fetch lands after the 0.02s cache replay and overwrites it — the panel
    // would go backwards whenever both ran, which is every time it opens.
    property int seq: 0
    property int appliedSeq: 0
    property int fullQueued: 0

    // `busy` is the network check only. The cache replay is 20ms and is not
    // worth telling anyone about.
    readonly property bool busy: statusProcess.running
    readonly property bool refreshing: statusProcess.running || quickProcess.running
    readonly property string state: Model.state(snap)
    readonly property string barText: Model.barText(snap)

    // The network check. An explicit ask is remembered rather than dropped: the
    // panel asks for one every time it opens, and "I pressed it and nothing
    // happened" is the whole complaint this file now answers.
    function refresh() {
        if (statusProcess.running) { fullQueued = 1; return }
        fullQueued = 0
        seq += 1
        statusProcess.seq = seq
        statusProcess.command = [bin, "status", "--json"]
        statusProcess.running = true
    }

    // The cache replay, on its own process so the network check can never
    // block it — and so it can never block the network check either.
    function refreshQuick() {
        if (quickProcess.running) return
        seq += 1
        quickProcess.seq = seq
        quickProcess.command = [bin, "status", "--no-fetch", "--json"]
        quickProcess.running = true
    }

    // One place that decides whether a result is still worth showing.
    function applyStatus(text, forSeq) {
        if (forSeq < appliedSeq) return
        appliedSeq = forSeq
        root.snap = Model.parse(text)
        root.loaded = true
        root.available = root.snap.ok
        root.lastError = root.snap.ok ? "" : "tempo-hours returned nothing usable"
    }

    // LAUNCHING MUST USE execDetached, NOT A Process.
    //
    // A Process is bound to the shell's lifetime: it runs the command, the
    // command execs `setsid` and exits 0, and the terminal it was trying to
    // start never survives. Everything reports success — exit 0, no stderr —
    // and no window appears. Quickshell.execDetached is the fire-and-forget
    // path, and it is what the bar's own `run()` helper uses underneath.
    function openTempo() {
        // The one action the tool deliberately refuses to automate: submitting.
        Quickshell.execDetached(["xdg-open", root.tempoUrl])
    }

    // SINGLE-WORD COMMANDS ONLY.
    // omarchy-launch-or-focus-tui builds LAUNCH_COMMAND="... $@", flattening
    // every argument into one string, so quoting does not survive it and
    // `bash -lc "tempo-hours review"` runs tempo-hours with no arguments. Both
    // of these are one-word wrapper scripts on PATH for exactly that reason.
    // The GO button. Whether this writes or merely drafts is decided by the
    // armed bit inside the tool, not here — one button, and the safety lives
    // where it belongs.
    function runNow(dry) { launch("tempo-hours-run") }

    // One place that knows how to open a terminal. `name` is both the app-id
    // and the command, which is why the wrappers are single words: the
    // launcher flattens its arguments into one string and only a bare command
    // survives that.
    function launch(name) {
        Quickshell.execDetached(["omarchy-launch-or-focus-tui", "--app-id=" + name, name])
    }

    function capture() { launch("tempo-hours-capture") }

    function review() { launch("tempo-hours-review") }

    property string tempoUrl:
        "https://agileenginecloud.atlassian.net/plugins/servlet/ac/io.tempo.jira/tempo-app#!/my-work/timesheet"

    // The slow one: goes to Tempo.
    property Process statusProcess: Process {
        property int seq: 0
        stdout: StdioCollector {
            onStreamFinished: root.applyStatus(this.text, statusProcess.seq)
        }
        stderr: StdioCollector {
            onStreamFinished: { if (this.text && !root.snap.ok) root.lastError = this.text.trim() }
        }
        onExited: function (code) {
            if (code !== 0 && code !== 13) {   // 13 = nothing owed, a fine answer
                root.available = false
                if (!root.lastError) root.lastError = "exit " + code
            }
            // An ask that arrived mid-flight is honoured now rather than lost.
            if (root.fullQueued === 1 && root.available) {
                root.fullQueued = 0
                root.refresh()
            }
        }
    }

    // The fast one: replays the cache, no network, so it runs alongside.
    property Process quickProcess: Process {
        property int seq: 0
        stdout: StdioCollector {
            onStreamFinished: root.applyStatus(this.text, quickProcess.seq)
        }
    }

    // No Process for launching. execDetached is fire-and-forget by design:
    // there is no exit code to collect, because the point is that what it
    // starts outlives this call.

    property Timer fullRefresh: Timer {
        interval: root.refreshIntervalSec * 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
