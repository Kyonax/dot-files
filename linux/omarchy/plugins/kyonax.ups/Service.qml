import QtQuick
import Quickshell
import Quickshell.Io

// Service.qml — reads NUT and the journal, holds the parsed state, and runs
// the few commands the popup offers.
//
// ONE Process PER READ, each on its own cadence, because they cost different
// things and fail independently — a dead notifier must not blank the readings:
//   upsc ups@localhost        local socket, ~5 ms   every refreshIntervalSec; every 2 s on battery
//   bin/ups-probe             systemctl, ~30 ms     every 30 s
//   journalctl -t ups-event   ~30 ms                every 60 s, and 3 s after any mains/battery flip
//   /etc/nut/upssched.conf    one small file        every 10 min
// The popup calls refresh() when it opens, so it never opens on old numbers.
//
// LAUNCHING uses Quickshell.execDetached, never a Process: a Process is bound
// to the shell, so the terminal it starts dies with it and every surface still
// reports success (paid for once in kyonax.tempo-hours). Reads stay Processes,
// because their output is the point.

import "Model.js" as Model

QtObject {
    id: root

    property int refreshIntervalSec: 5
    // Absolute paths of the helpers next to Panel.qml, handed in by the Panel,
    // which is the one that knows where the plugin lives.
    property string helper: ""
    property string probe: ""
    readonly property string credPath: Quickshell.env("HOME") + "/.config/kyonax-ups/upsd-widget"

    property var reading: Model.reading(Model.parseUpsc(""))
    property var services: Model.parseServices("")
    property var events: []
    property var policy: Model.parsePolicy("")
    property bool loaded: false

    // The outage start as this widget WATCHED it happen (OL -> OB), or null
    // when it never saw the flip (it started mid-outage). See Model.outageStart.
    property var obSeenAt: null
    property var lastOnBattery: null
    property real nowMs: Date.now()

    // The last command's answer, shown under the action row for a few seconds.
    property string feedback: ""
    property bool feedbackError: false

    readonly property var protection: Model.protection(reading, services)
    readonly property var countdown: Model.countdown(reading, events, policy, nowMs, obSeenAt)
    readonly property bool shuttingDown: Model.shuttingDown(events, reading, nowMs)
    readonly property string uiState: Model.state(reading, protection)
    readonly property var outageList: Model.outages(events, nowMs, reading.ok && reading.onBattery)
    readonly property bool controlling: instProcess.running

    function refresh() { readUps(); readServices(); readEvents(); readPolicy() }

    function readUps() { if (!upsProcess.running) upsProcess.running = true }

    function readServices() {
        if (servicesProcess.running || !root.probe) return
        servicesProcess.command = ["bash", root.probe, root.credPath]
        servicesProcess.running = true
    }

    function readEvents() { if (!eventsProcess.running) eventsProcess.running = true }
    function readPolicy() { if (!policyProcess.running) policyProcess.running = true }

    function applyUps(text) {
        var r = Model.reading(Model.parseUpsc(text))
        var now = Date.now()
        if (r.ok) {
            var ob = r.onBattery
            if (root.lastOnBattery === false && ob) root.obSeenAt = now
            if (!ob) root.obSeenAt = null
            // upssched writes its journal line a poll or two after the flip.
            if (root.lastOnBattery !== null && root.lastOnBattery !== ob) eventsSoon.restart()
            root.lastOnBattery = ob
        }
        root.nowMs = now
        root.reading = r
        root.loaded = true
    }

    // The only commands the popup may send to the UPS itself. upsd refuses
    // anything else for this user (setup/setup-widget-access.sh), and the
    // helper refuses anything else before asking — two locks, not one.
    function toggleBeeper() {
        instcmd("beeper.toggle", Model.beeperOn(root.reading) ? "Beeper muted" : "Beeper on")
    }

    function startTest() {
        instcmd("test.battery.start.quick", "Battery test started: expect On battery, then Power back")
    }

    function instcmd(cmd, label) {
        if (instProcess.running) return
        if (!root.helper) { say("the control helper is missing", true); return }
        instProcess.label = label
        instProcess.command = ["bash", root.helper, "--cred", root.credPath, cmd]
        instProcess.running = true
    }

    function say(text, isError) {
        root.feedback = text
        root.feedbackError = !!isError
        feedbackClear.restart()
    }

    // SINGLE-WORD COMMAND ON PATH: omarchy-launch-or-focus-tui flattens its
    // arguments into one string, so only a bare command survives the trip.
    function openLog() {
        Quickshell.execDetached(["omarchy-launch-or-focus-tui", "--app-id=ups-log", "ups-log"])
    }

    // Omarchy's own shutdown: it closes app windows first, so browsers and
    // editors save their state, then powers off.
    function shutdownNow() { Quickshell.execDetached(["omarchy-system-shutdown"]) }

    property Process upsProcess: Process {
        command: ["sh", "-c", "upsc ups@localhost 2>&1"]
        stdout: StdioCollector { onStreamFinished: root.applyUps(this.text) }
    }

    property Process servicesProcess: Process {
        stdout: StdioCollector { onStreamFinished: root.services = Model.parseServices(this.text) }
    }

    property Process eventsProcess: Process {
        command: ["journalctl", "-t", "ups-event", "-o", "json",
                  "--output-fields=MESSAGE,UPS_EVENT", "-n", "60", "--no-pager"]
        stdout: StdioCollector { onStreamFinished: root.events = Model.parseEvents(this.text) }
    }

    property Process policyProcess: Process {
        command: ["cat", "/etc/nut/upssched.conf"]
        stdout: StdioCollector { onStreamFinished: root.policy = Model.parsePolicy(this.text) }
    }

    // The helper prints OK or ERR <REASON>, so its stdout alone decides —
    // no race between the exit code and the stream.
    property Process instProcess: Process {
        property string label: ""
        stdout: StdioCollector {
            onStreamFinished: {
                var res = Model.instcmdResult(this.text, instProcess.label)
                root.say(res.text, !res.ok)
                afterCommand.restart()
            }
        }
    }

    property Timer upsTimer: Timer {
        interval: (root.reading.ok && root.reading.onBattery ? 2 : Math.max(2, root.refreshIntervalSec)) * 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.readUps()
    }

    property Timer servicesTimer: Timer {
        interval: 30000; running: true; repeat: true; triggeredOnStart: true
        onTriggered: root.readServices()
    }

    property Timer eventsTimer: Timer {
        interval: 60000; running: true; repeat: true; triggeredOnStart: true
        onTriggered: root.readEvents()
    }

    property Timer policyTimer: Timer {
        interval: 600000; running: true; repeat: true; triggeredOnStart: true
        onTriggered: root.readPolicy()
    }

    // The countdown ticks every second on battery, between readings.
    property Timer ticker: Timer {
        interval: 1000
        running: root.reading.ok && root.reading.onBattery
        repeat: true
        onTriggered: root.nowMs = Date.now()
    }

    property Timer eventsSoon: Timer { interval: 3000; onTriggered: root.readEvents() }

    // Re-read a moment after a command, so its effect (beeper, test) shows.
    property Timer afterCommand: Timer {
        interval: 1500
        onTriggered: { root.readUps(); root.readEvents() }
    }

    property Timer feedbackClear: Timer { interval: 9000; onTriggered: root.feedback = "" }
}
