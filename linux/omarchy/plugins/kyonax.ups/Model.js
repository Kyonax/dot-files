.pragma library

// Model.js — the UPS widget's pure logic: parse, decide, word.
//
// No QML types, no I/O, and no clock of its own: every function is handed what
// it needs (including `nowMs`), so all of it runs under `node Model.test.mjs`.
// The question that matters in an outage — how long until the PC shuts itself
// down? — is answered by tested arithmetic, not by the panel.
//
// Inputs, all gathered by Service.qml:
//   upsc ups@localhost 2>&1          "key: value" lines, or "Error: <why>"
//   bin/ups-probe                    "name=state" lines (systemctl is-active)
//   journalctl -t ups-event -o json  one object per line; UPS_EVENT is the
//                                    machine key written by /etc/nut/upssched-cmd
//   /etc/nut/upssched.conf           the on-battery timer, parsed, never assumed

// Material Design glyphs from the Nerd Font the bar already uses. Code points,
// not pasted characters, so an editor cannot silently mangle them.
var G = {
    plug:        String.fromCodePoint(0xF06A5),   // on mains
    plugOff:     String.fromCodePoint(0xF06A6),   // no readings
    battAlert:   String.fromCodePoint(0xF0083),   // low battery
    charging:    String.fromCodePoint(0xF0084),
    battUnknown: String.fromCodePoint(0xF0091),
    battEmpty:   String.fromCodePoint(0xF008E),
    battFull:    String.fromCodePoint(0xF0079),
    bell:        String.fromCodePoint(0xF009A),
    bellOff:     String.fromCodePoint(0xF009B),
    power:       String.fromCodePoint(0xF0425),
    test:        String.fromCodePoint(0xF0668),
    refresh:     String.fromCodePoint(0xF0450),
    log:         String.fromCodePoint(0xF0219),
    shieldOff:   String.fromCodePoint(0xF099E)
}

// ---------------------------------------------------------------- parsing

function num(v) {
    if (v === undefined || v === null || v === "") return null
    var n = parseFloat(v)
    return isNaN(n) ? null : n
}

// upsc prints "key: value" per line, or "Error: <why>" when upsd or the driver
// is down. Anything else (a blank line, a shell error) is kept as the error
// text rather than trusted as a reading.
function parseUpsc(text) {
    var vars = {}
    var lines = String(text || "").split("\n")
    for (var i = 0; i < lines.length; i++) {
        var line = lines[i]
        var k = line.indexOf(":")
        if (k <= 0) continue
        var key = line.slice(0, k).trim()
        if (!/^[A-Za-z][\w.]*$/.test(key)) continue
        vars[key] = line.slice(k + 1).trim()
    }
    var ok = !!vars["ups.status"]
    var error = ""
    if (!ok) error = vars["Error"] || String(text || "").trim() || "no answer from upsd"
    return { ok: ok, vars: vars, error: error }
}

function reading(p) {
    var v = (p && p.vars) || {}
    var status = v["ups.status"] || ""
    var flags = status.split(/\s+/).filter(function (x) { return x.length > 0 })
    function has(f) { return flags.indexOf(f) !== -1 }
    return {
        ok: !!(p && p.ok),
        error: (p && p.error) || "",
        status: status,
        flags: flags,
        online: has("OL"),
        onBattery: has("OB"),
        lowBattery: has("LB"),
        charging: has("CHRG"),
        replaceBattery: has("RB"),
        overload: has("OVER"),
        testing: has("CAL"),
        waiting: has("WAIT"),
        forced: has("FSD"),
        alarm: has("ALARM"),
        inputV: num(v["input.voltage"]),
        inputHz: num(v["input.frequency"]),
        inputNominal: num(v["input.voltage.nominal"]),
        outputV: num(v["output.voltage"]),
        load: num(v["ups.load"]),
        charge: num(v["battery.charge"]),
        battV: num(v["battery.voltage"]),
        battNominal: num(v["battery.voltage.nominal"]),
        battLow: num(v["battery.voltage.low"]),
        temp: num(v["ups.temperature"]),
        beeper: v["ups.beeper.status"] || "",
        type: v["ups.type"] || "",
        firmware: v["ups.firmware"] || ""
    }
}

// bin/ups-probe: the three NUT units, the notifier, and whether the control
// credential exists (cred=yes|no).
function parseServices(text) {
    var s = { known: false, driver: "unknown", server: "unknown", monitor: "unknown",
              notify: "unknown", cred: false }
    var names = { "nut-driver@ups": "driver", "nut-server": "server",
                  "nut-monitor": "monitor", "ups-notify": "notify" }
    var lines = String(text || "").split("\n")
    for (var i = 0; i < lines.length; i++) {
        var m = /^([\w@.-]+)=(\S*)/.exec(lines[i].trim())
        if (!m) continue
        if (m[1] === "cred") { s.cred = m[2] === "yes"; s.known = true }
        else if (names[m[1]]) { s[names[m[1]]] = m[2] || "unknown"; s.known = true }
    }
    return s
}

// The on-battery timer as upssched is ACTUALLY configured. With no timer the
// PC only goes down at low battery, and the panel says so instead of
// inventing a countdown.
function parsePolicy(text) {
    var p = { known: false, timerSec: null }
    var lines = String(text || "").split("\n")
    for (var i = 0; i < lines.length; i++) {
        var line = lines[i].replace(/#.*/, "").trim()
        if (!line) continue
        p.known = true
        var m = /^AT\s+ONBATT\s+\S+\s+START-TIMER\s+\S+\s+(\d+)$/.exec(line)
        if (m) p.timerSec = parseInt(m[1], 10)
    }
    return p
}

var EVENT_KEYS = ["onbatt", "online", "timeout", "lowbatt", "shutdown",
                  "commbad", "commok", "replbatt", "test", "other"]

// UPS_EVENT is authoritative. The prose fallback only covers lines written
// before upssched-cmd started tagging them.
function eventKey(msg, field) {
    if (field && EVENT_KEYS.indexOf(field) !== -1) return field
    var m = String(msg || "")
    if (/^On battery for /.test(m)) return "timeout"
    if (/^On battery/.test(m)) return "onbatt"
    if (/^Power back/.test(m)) return "online"
    if (/^UPS battery low/.test(m)) return "lowbatt"
    if (/^UPS: clean system shutdown/.test(m)) return "shutdown"
    if (/^UPS: lost contact/.test(m)) return "commbad"
    if (/^UPS: contact restored/.test(m)) return "commok"
    if (/^UPS: the battery needs replacing/.test(m)) return "replbatt"
    if (/^Test\b/.test(m)) return "test"
    return "other"
}

function parseEvents(text) {
    var out = []
    var lines = String(text || "").split("\n")
    for (var i = 0; i < lines.length; i++) {
        var line = lines[i].trim()
        if (!line) continue
        var j
        try { j = JSON.parse(line) } catch (e) { continue }
        var us = parseInt(j.__REALTIME_TIMESTAMP, 10)
        if (isNaN(us)) continue
        var msg = typeof j.MESSAGE === "string" ? j.MESSAGE : ""
        out.push({ t: Math.floor(us / 1000), key: eventKey(msg, j.UPS_EVENT), msg: msg })
    }
    out.sort(function (a, b) { return a.t - b.t })
    return out
}

// ---------------------------------------------------------------- decisions

// Protected = readings flow AND the part that powers the PC off is running.
// The notifier is not part of it: a PC can be protected in silence, so a dead
// notifier is a note, never an alert. Before the first probe: no verdict.
function protection(r, s) {
    if (!s || !s.known) return { ok: true, known: false, reason: "" }
    if (s.monitor !== "active") return { ok: false, known: true, reason: "the shutdown monitor is " + s.monitor }
    if (s.driver !== "active") return { ok: false, known: true, reason: "the UPS driver is " + s.driver }
    if (s.server !== "active") return { ok: false, known: true, reason: "the NUT server is " + s.server }
    if (!r || !r.ok) return { ok: false, known: true, reason: "no readings from the UPS" }
    return { ok: true, known: true, reason: "" }
}

// The latest "on battery" not already closed by a later mains return or
// shutdown — i.e. the start of an outage still in progress, if any.
function lastOnbatt(events) {
    for (var i = (events || []).length - 1; i >= 0; i--) {
        var k = events[i].key
        if (k === "online" || k === "timeout" || k === "shutdown") return null
        if (k === "onbatt") return events[i].t
    }
    return null
}

// When did THIS outage start? The journal line from upssched is exact, but it
// can land a poll after the widget sees OB, and after a reboot the last
// "on battery" in the journal may be an old, unclosed one. So the journal
// wins when it agrees with what the widget watched (within a minute), the
// widget's own sighting wins when they disagree, and with no sighting (the
// widget started mid-outage) the journal is all there is.
function outageStart(events, seenMs) {
    var j = lastOnbatt(events)
    if (seenMs === null || seenMs === undefined) return j
    if (j !== null && Math.abs(j - seenMs) <= 60000) return j
    return seenMs
}

function countdown(r, events, policy, nowMs, seenMs) {
    if (!r || !r.ok || !r.onBattery) return null
    if (!policy || !policy.timerSec) return null
    var since = outageStart(events, seenMs)
    if (since === null) return null
    var total = policy.timerSec * 1000
    var deadline = since + total
    return {
        since: since,
        totalMs: total,
        deadlineMs: deadline,
        elapsedMs: Math.max(0, Math.min(total, nowMs - since)),
        remainingMs: Math.max(0, deadline - nowMs)
    }
}

// On battery and the timer (or low battery) has already fired: the PC is going
// down right now, and a countdown would be a lie.
function shuttingDown(events, r, nowMs) {
    if (!r || !r.ok || !r.onBattery) return false
    if (r.forced) return true
    var e = (events && events.length) ? events[events.length - 1] : null
    return !!(e && (e.key === "timeout" || e.key === "shutdown") && nowMs - e.t < 600000)
}

// The three states every widget in this bar speaks (see kyonax.tempo-hours):
// only `alert` earns the urgent colour; a settled UPS should not pull the eye.
function state(r, prot) {
    if (!r || !r.ok) return "alert"
    if (r.onBattery || r.lowBattery || r.replaceBattery || r.overload || r.forced || r.alarm)
        return "alert"
    if (prot && prot.known && !prot.ok) return "alert"
    if (r.waiting || r.testing || r.charging) return "pending"
    return "settled"
}

// Pair every "on battery" with what ended it. An outage the journal never saw
// end (the PC was off when power came back) has no duration, not a made-up one.
function outages(events, nowMs, onBatteryNow) {
    var list = [], open = null
    for (var i = 0; i < (events || []).length; i++) {
        var e = events[i]
        if (e.key === "onbatt") {
            if (open) list.push({ start: open, end: null, how: "unknown" })
            open = e.t
        } else if (open !== null && e.key === "online") {
            list.push({ start: open, end: e.t, how: "power" }); open = null
        } else if (open !== null && (e.key === "timeout" || e.key === "shutdown")) {
            list.push({ start: open, end: e.t, how: "shutdown" }); open = null
        }
    }
    if (open !== null) list.push({ start: open, end: null, how: onBatteryNow ? "ongoing" : "unknown" })
    for (var k = 0; k < list.length; k++) {
        var o = list[k]
        o.durationMs = o.end !== null ? o.end - o.start : (o.how === "ongoing" ? nowMs - o.start : null)
    }
    return list.reverse()
}

// ---------------------------------------------------------------- words

function pad2(n) { return (n < 10 ? "0" : "") + n }

function clock(ms) {                    // 161000 -> "2:41"
    var s = Math.max(0, Math.ceil((ms || 0) / 1000))
    return Math.floor(s / 60) + ":" + pad2(s % 60)
}

function duration(ms) {                 // 45000 -> "45s", 192000 -> "3m 12s"
    if (ms === null || ms === undefined || isNaN(ms)) return "—"
    var s = Math.max(0, Math.round(ms / 1000))
    if (s < 60) return s + "s"
    var m = Math.floor(s / 60)
    if (m < 60) return m + "m " + pad2(s % 60) + "s"
    var h = Math.floor(m / 60)
    if (h < 48) return h + "h " + pad2(m % 60) + "m"
    return Math.floor(h / 24) + "d " + (h % 24) + "h"
}

var DAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
var MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

function hhmm(d) { return pad2(d.getHours()) + ":" + pad2(d.getMinutes()) }

function clockTime(ms) {                // local "04:05:12"
    var d = new Date(ms)
    return hhmm(d) + ":" + pad2(d.getSeconds())
}

function when(ms, nowMs) {              // "today 04:02" / "yesterday 23:10" / "Tue 29 Sep 04:02"
    if (ms === null || ms === undefined) return "—"
    var d = new Date(ms), n = new Date(nowMs)
    var today = new Date(n.getFullYear(), n.getMonth(), n.getDate()).getTime()
    if (ms >= today) return "today " + hhmm(d)
    if (ms >= today - 86400000) return "yesterday " + hhmm(d)
    return DAYS[d.getDay()] + " " + d.getDate() + " " + MONTHS[d.getMonth()] + " " + hhmm(d)
}

function trim1(n) { return Math.abs(n - Math.round(n)) < 0.05 ? String(Math.round(n)) : n.toFixed(1) }
function volts(n) { return n === null ? "—" : trim1(n) + " V" }
function hz(n) { return n === null ? "—" : trim1(n) + " Hz" }
function pct(n) { return n === null ? "—" : Math.round(n) + "%" }
function celsius(n) { return n === null ? "—" : n.toFixed(1) + " °C" }

// 󰁺 … 󰂂 are battery-10 … battery-90 in steps of ten; 󰁹 is full.
function batteryGlyph(charge) {
    if (charge === null || charge === undefined || isNaN(charge)) return G.battUnknown
    if (charge >= 95) return G.battFull
    if (charge < 5) return G.battEmpty
    var step = Math.max(1, Math.min(9, Math.round(charge / 10)))
    return String.fromCodePoint(0xF007A + step - 1)
}

function barGlyph(r, prot) {
    if (!r || !r.ok) return G.plugOff
    if (r.lowBattery) return G.battAlert
    if (r.onBattery) return batteryGlyph(r.charge)
    if (prot && prot.known && !prot.ok) return G.shieldOff
    if (r.charging) return G.charging
    return G.plug
}

// What sits next to the glyph. On battery the countdown is the only thing that
// matters; on mains the owner picks (settings.barLabel), and silence is the
// default because a healthy UPS has nothing to say.
function barLabel(r, prot, cd, mode, goingDown) {
    if (!r || !r.ok) return "UPS?"
    if (r.onBattery) {
        if (goingDown) return "shutdown"
        if (r.lowBattery) return "LOW"
        return cd ? clock(cd.remainingMs) : "battery"
    }
    if (prot && prot.known && !prot.ok) return "unprotected"
    if (r.testing) return "test"
    if (mode === "Load") return r.load === null ? "" : Math.round(r.load) + "%"
    if (mode === "Input voltage") return r.inputV === null ? "" : Math.round(r.inputV) + "V"
    return ""
}

function title(r, cd, goingDown) {
    if (!r || !r.ok) return "UPS not answering"
    if (r.onBattery && goingDown) return "Shutting down now"
    if (r.onBattery && r.lowBattery) return "Battery low"
    if (r.onBattery) return cd ? "On battery · " + clock(cd.remainingMs) : "On battery"
    if (r.testing) return "Battery test running"
    if (r.waiting) return "UPS driver starting"
    return "On mains"
}

function policyText(policy) {
    if (!policy || !policy.known) return "shutdown policy unreadable"
    if (!policy.timerSec) return "shuts down at low battery only"
    return "clean shutdown after " + clock(policy.timerSec * 1000) + " on battery"
}

// PanelHero puts the title and this pill on ONE row and never caps the pill,
// so a long pill squeezes the title to nothing (seen 2026-09-29: "On mains"
// vanished behind "protected · clean shutdown after 3:00 on battery").
// The pill is one word or two, always.
function heroPill(prot) {
    if (!prot || !prot.known) return "checking"
    return prot.ok ? "protected" : "NOT protected"
}

// The hero's second line, which PanelHero upper-cases, letter-spaces and
// elides after roughly 36 characters: the ONE sentence that matters now.
// The electrical numbers live in the tiles right below it.
function heroMeta(r, prot, policy, cd, goingDown) {
    if (!r || !r.ok) return (r && r.error) || "no readings"
    if (prot && prot.known && !prot.ok) return prot.reason
    if (r.onBattery && goingDown) return "powering off now"
    if (r.onBattery && cd) return "clean shutdown at " + clockTime(cd.deadlineMs)
    if (r.onBattery) return "no mains input"
    if (!policy || !policy.known) return "shutdown policy unreadable"
    if (!policy.timerSec) return "shuts down at low battery only"
    return "shuts down after " + clock(policy.timerSec * 1000) + " on battery"
}

function tooltip(r, prot, cd) {
    if (!r || !r.ok) return "UPS: " + ((r && r.error) || "no readings")
    var parts = [title(r, cd, false)]
    if (!r.onBattery && r.inputV !== null) parts.push(volts(r.inputV) + " in")
    if (r.load !== null) parts.push("load " + pct(r.load))
    if (r.onBattery && r.charge !== null) parts.push("battery " + pct(r.charge))
    if (prot && prot.known) parts.push(prot.ok ? "protected" : "NOT protected")
    return parts.join(" · ")
}

function tiles(r) {
    if (!r || !r.ok) return []
    return [
        { value: pct(r.load), label: "Load", alert: r.load !== null && r.load >= 80 },
        { value: r.charge !== null ? pct(r.charge) : volts(r.battV), label: "Battery",
          alert: r.lowBattery || (r.onBattery && r.charge !== null && r.charge < 40) },
        { value: r.onBattery ? "off" : volts(r.inputV), label: "Mains", alert: r.onBattery },
        { value: celsius(r.temp), label: "UPS temp", alert: r.temp !== null && r.temp >= 45 }
    ]
}

function beeperText(r) {
    var b = r && r.beeper
    if (b === "enabled") return "on"
    if (b === "disabled") return "off"
    if (b === "muted") return "muted"
    return b || "—"
}

function beeperOn(r) { return !!(r && r.beeper === "enabled") }

function detailRows(r) {
    if (!r || !r.ok) return []
    var input = r.inputV === null ? "—"
        : volts(r.inputV) + (r.inputHz !== null ? " · " + hz(r.inputHz) : "")
          + (r.inputNominal !== null ? "  (nominal " + volts(r.inputNominal) + ")" : "")
    var batt = r.battV === null ? "—"
        : r.battV.toFixed(1) + " V" + (r.battNominal !== null ? " · " + volts(r.battNominal) + " nominal" : "")
          + (r.battLow !== null ? " · low " + r.battLow.toFixed(1) + " V" : "")
    return [
        { label: "Input", value: input },
        { label: "Output", value: volts(r.outputV) },
        { label: "Battery", value: batt },
        { label: "Charge", value: r.charge === null ? "—" : pct(r.charge) + " (estimated from voltage)" },
        { label: "Beeper", value: beeperText(r) },
        { label: "UPS", value: r.status + (r.type ? " · " + r.type : "")
                             + (r.firmware ? " · fw " + r.firmware : "") }
    ]
}

function serviceRows(s) {
    function word(st) { return st === "active" ? "running" : (st || "unknown") }
    return [
        { label: "Shutdown monitor", value: word(s.monitor), alert: s.known && s.monitor !== "active" },
        { label: "UPS driver", value: word(s.driver), alert: s.known && s.driver !== "active" },
        { label: "NUT server", value: word(s.server), alert: s.known && s.server !== "active" },
        { label: "Notifications", value: s.notify === "active" ? "on" : word(s.notify), alert: false }
    ]
}

var EVENT_TEXT = {
    onbatt: "On battery", online: "Power back", timeout: "Timer ran out · shutting down",
    lowbatt: "Battery low", shutdown: "Clean shutdown", commbad: "Lost contact with the UPS",
    commok: "Contact restored", replbatt: "Battery needs replacing", test: "Pipeline test"
}
var EVENT_ALERT = ["onbatt", "timeout", "lowbatt", "shutdown", "commbad", "replbatt"]

// Power history only: the pipeline self-test lines prove the notifier works,
// but they are not outages and would crowd the list.
function eventRows(events, nowMs, limit) {
    var out = []
    var max = limit || 6
    for (var i = (events || []).length - 1; i >= 0 && out.length < max; i--) {
        var e = events[i]
        if (e.key === "test") continue
        out.push({ when: when(e.t, nowMs), text: EVENT_TEXT[e.key] || e.msg || e.key,
                   alert: EVENT_ALERT.indexOf(e.key) !== -1 })
    }
    return out
}

function lastOutageText(list, nowMs) {
    if (!list || !list.length) return "none logged yet"
    var o = list[0]
    var how = o.how === "power" ? "power came back"
            : o.how === "shutdown" ? "shut down cleanly"
            : o.how === "ongoing" ? "happening now" : "end not seen"
    return when(o.start, nowMs) + " · " + duration(o.durationMs) + " · " + how
}

// ---------------------------------------------------------------- controls

function canControl(r, s) { return !!(r && r.ok && s && s.cred) }

function controlHint(r, s, pluginDir) {
    if (!r || !r.ok || !s || !s.known || s.cred) return ""
    return "Beeper and battery-test buttons need a one-time setup:  sudo sh "
         + pluginDir + "/setup/setup-widget-access.sh"
}

// bin/ups-instcmd prints OK or ERR <REASON> (upsd's own tokens, or its own).
var REASONS = {
    "ACCESS-DENIED": "access denied (re-run the widget setup)",
    "INVALID-PASSWORD": "the widget password is wrong (re-run the widget setup)",
    "CMD-NOT-SUPPORTED": "this UPS does not support it",
    "UNKNOWN-UPS": "upsd does not know the UPS",
    "DRIVER-NOT-CONNECTED": "the UPS driver is not connected",
    "no-credential": "needs the one-time widget setup",
    "not-allowed": "not a command the widget may send",
    "upsd-unreachable": "upsd is not reachable",
    "timeout": "upsd did not answer"
}

function instcmdResult(out, label) {
    var lines = String(out || "").trim().split("\n")
    var t = lines[lines.length - 1].trim()
    if (/^OK\b/.test(t)) return { ok: true, text: label }
    var m = /^ERR\s+(\S+)/.exec(t)
    var reason = m ? m[1] : (t || "no answer")
    return { ok: false, text: label + " failed: " + (REASONS[reason] || reason.toLowerCase()) }
}
