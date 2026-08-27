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
// Process.running is the anti-stampede guard. Without it a fast poll would
// spawn a second CLI before the first returned and pile up requests.

import "Model.js" as Model

QtObject {
    id: root

    property string bin: "tempo-hours"
    property int refreshIntervalSec: 900

    property var snap: Model.parse("")
    property bool loaded: false
    property bool refreshing: false
    property string lastError: ""
    property bool available: true

    readonly property bool busy: statusProcess.running
    readonly property string state: Model.state(snap)
    readonly property string barText: Model.barText(snap)

    function refresh() { start(false) }
    function refreshQuick() { start(true) }

    function start(noFetch) {
        if (statusProcess.running) return
        refreshing = true
        statusProcess.command = noFetch
            ? [bin, "status", "--no-fetch", "--json"]
            : [bin, "status", "--json"]
        statusProcess.running = true
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

    property Process statusProcess: Process {
        stdout: StdioCollector {
            onStreamFinished: {
                root.snap = Model.parse(this.text)
                root.loaded = true
                root.available = root.snap.ok
                root.lastError = root.snap.ok ? "" : "tempo-hours returned nothing usable"
                root.refreshing = false
            }
        }
        stderr: StdioCollector {
            onStreamFinished: { if (this.text && !root.snap.ok) root.lastError = this.text.trim() }
        }
        onExited: function (code) {
            root.refreshing = false
            if (code !== 0 && code !== 13) {   // 13 = nothing owed, a fine answer
                root.available = false
                if (!root.lastError) root.lastError = "exit " + code
            }
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
