// Model.test.mjs — the UPS widget's logic, checked without a shell.
//
//   node Model.test.mjs
//
// Same harness as kyonax.shipwright: `.pragma library` is a QML directive, not
// JavaScript, so it is stripped before evaluation. Nothing else is transformed.
// The fixtures are this machine's real output (2026-09-29): a Richcomm
// 0925:1234 board on nutdrv_qx, and the upssched.conf installed that day.

process.env.TZ = "America/Bogota"

import { readFileSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"

const here = dirname(fileURLToPath(import.meta.url))
const src = readFileSync(join(here, "Model.js"), "utf8").replace(/^\.pragma library/m, "")
const names = ["G", "parseUpsc", "reading", "parseServices", "parsePolicy", "parseEvents",
  "eventKey", "protection", "lastOnbatt", "outageStart", "countdown", "shuttingDown", "state",
  "outages", "clock", "duration", "when", "clockTime", "volts", "batteryGlyph", "barGlyph",
  "barLabel", "title", "heroMeta", "heroPill", "tooltip", "tiles", "detailRows", "serviceRows",
  "eventRows", "lastOutageText", "canControl", "controlHint", "instcmdResult", "beeperOn"]
const M = {}
new Function("exports", src + "\n;Object.assign(exports, {" + names.join(", ") + "});")(M)

let pass = 0
const fails = []
function is(name, got, want) {
  const g = JSON.stringify(got), w = JSON.stringify(want)
  if (g === w) { pass++; console.log("  ok    " + name) }
  else { fails.push(name); console.log("  FAIL  " + name + "\n          want: " + w + "\n          got:  " + g) }
}
const cp = (s) => s.codePointAt(0).toString(16).toUpperCase()

// ---------------------------------------------------------------- fixtures

const UPSC_OL = `battery.charge: 100
battery.voltage: 26.8
battery.voltage.high: 26.00
battery.voltage.low: 20.80
battery.voltage.nominal: 24.0
device.serial:
device.type: ups
driver.name: nutdrv_qx
input.frequency: 60.0
input.voltage: 122.0
input.voltage.nominal: 110
output.voltage: 122.0
ups.beeper.status: enabled
ups.delay.shutdown: 30
ups.firmware: V3.65
ups.load: 18
ups.status: OL
ups.temperature: 20.8
ups.type: offline / line interactive`

const UPSC_OB = UPSC_OL.replace("ups.status: OL", "ups.status: OB")
  .replace("battery.charge: 100", "battery.charge: 57").replace("input.voltage: 122.0", "input.voltage: 0.0")
const SCHED = `# /etc/nut/upssched.conf (kyo-labs, 2026-09-29).
CMDSCRIPT /etc/nut/upssched-cmd
PIPEFN /run/nut/upssched.pipe
AT ONBATT * START-TIMER onbatt-3min 180
AT ONLINE * CANCEL-TIMER onbatt-3min
AT ONBATT   * EXECUTE onbatt`
const PROBE_OK = "nut-driver@ups=active\nnut-server=active\nnut-monitor=active\nups-notify=active\ncred=no\n"
const T0 = Date.parse("2026-09-29T04:02:00-05:00")
const ev = (ms, key, msg) => JSON.stringify({ __REALTIME_TIMESTAMP: String(ms * 1000), MESSAGE: msg || key, UPS_EVENT: key })

// ---------------------------------------------------------------- parsing

console.log("parsing")
const ol = M.reading(M.parseUpsc(UPSC_OL))
is("upsc OL parses", [ol.ok, ol.status, ol.online, ol.onBattery], [true, "OL", true, false])
is("numbers parse", [ol.inputV, ol.inputHz, ol.load, ol.battV, ol.charge, ol.temp], [122, 60, 18, 26.8, 100, 20.8])
is("empty value is null, not 0", M.reading(M.parseUpsc("device.serial:\nups.status: OL")).load, null)
const down = M.reading(M.parseUpsc("Error: Driver not connected"))
is("driver down is an error, not a reading", [down.ok, down.error], [false, "Driver not connected"])
is("shell failure keeps its text", M.parseUpsc("sh: line 1: upsc: command not found").error,
   "sh: line 1: upsc: command not found")
is("empty output", M.parseUpsc("").error, "no answer from upsd")
const oblb = M.reading(M.parseUpsc("ups.status: OB LB"))
is("OB LB flags", [oblb.onBattery, oblb.lowBattery, oblb.online], [true, true, false])
is("WAIT at driver start", M.reading(M.parseUpsc("ups.status: WAIT")).waiting, true)

const svc = M.parseServices(PROBE_OK)
is("probe parses", [svc.known, svc.driver, svc.server, svc.monitor, svc.notify, svc.cred],
   [true, "active", "active", "active", "active", false])
is("probe: empty state is unknown", M.parseServices("nut-monitor=\n").monitor, "unknown")
is("probe: nothing yet", M.parseServices("").known, false)

is("policy: the real timer", M.parsePolicy(SCHED), { known: true, timerSec: 180 })
is("policy: commented timer means none", M.parsePolicy("# AT ONBATT * START-TIMER x 180\nCMDSCRIPT /x"),
   { known: true, timerSec: null })
is("policy: unreadable file", M.parsePolicy(""), { known: false, timerSec: null })

const evs = M.parseEvents([ev(T0 + 60000, "online"), ev(T0, "onbatt"), "not json", ""].join("\n"))
is("events sorted, junk skipped", evs.map(e => e.key), ["onbatt", "online"])
is("legacy prose lines still classify", [
  M.eventKey("On battery: mains power lost. Clean shutdown in 3 minutes unless power returns."),
  M.eventKey("On battery for 3 minutes: shutting down cleanly now."),
  M.eventKey("Power back: running on mains again. Shutdown cancelled."),
  M.eventKey("Test: UPS notifications are working (from the setup, ignore).")],
  ["onbatt", "timeout", "online", "test"])
is("unknown UPS_EVENT falls back to prose", M.eventKey("Power back: x", "bogus"), "online")

// ---------------------------------------------------------------- decisions

console.log("decisions")
const prot = M.protection(ol, svc)
is("protected when all runs", prot, { ok: true, known: true, reason: "" })
is("monitor down = not protected",
   M.protection(ol, M.parseServices(PROBE_OK.replace("nut-monitor=active", "nut-monitor=failed"))).reason,
   "the shutdown monitor is failed")
is("no probe yet = no verdict", M.protection(ol, M.parseServices("")).known, false)
is("no readings = not protected", M.protection(down, svc).reason, "no readings from the UPS")

is("settled on mains", M.state(ol, prot), "settled")
is("alert on battery", M.state(M.reading(M.parseUpsc(UPSC_OB)), prot), "alert")
is("alert when unprotected", M.state(ol, { ok: false, known: true, reason: "x" }), "alert")
is("pending while the driver starts", M.state(M.reading(M.parseUpsc("ups.status: WAIT")), prot), "pending")
is("alert with no readings", M.state(down, prot), "alert")

const ob = M.reading(M.parseUpsc(UPSC_OB))
const outageEvents = [{ t: T0, key: "onbatt", msg: "" }]
const pol = M.parsePolicy(SCHED)
const cd = M.countdown(ob, outageEvents, pol, T0 + 19000, T0 + 2000)
is("countdown from the journal line (agrees with sighting)", [cd.remainingMs, cd.deadlineMs - T0], [161000, 180000])
is("countdown clock text", M.clock(cd.remainingMs), "2:41")
is("stale journal loses to the sighting",
   M.outageStart([{ t: T0 - 86400000, key: "onbatt" }], T0), T0)
is("started mid-outage: journal is all there is", M.outageStart(outageEvents, null), T0)
is("closed outage is not current", M.lastOnbatt([{ t: T0, key: "onbatt" }, { t: T0 + 5, key: "online" }]), null)
is("no countdown without a timer", M.countdown(ob, outageEvents, { known: true, timerSec: null }, T0, null), null)
is("no countdown on mains", M.countdown(ol, outageEvents, pol, T0, null), null)
is("countdown never goes negative", M.countdown(ob, outageEvents, pol, T0 + 999999, null).remainingMs, 0)
is("timer fired = shutting down", M.shuttingDown([...outageEvents, { t: T0 + 180000, key: "timeout" }], ob, T0 + 181000), true)
is("on battery alone is not shutting down", M.shuttingDown(outageEvents, ob, T0 + 1000), false)

const hist = [
  { t: T0 - 7200000, key: "onbatt" }, { t: T0 - 7100000, key: "online" },
  { t: T0 - 3600000, key: "onbatt" }, { t: T0 - 3420000, key: "timeout" }, { t: T0 - 3419000, key: "shutdown" },
  { t: T0 - 60000, key: "onbatt" }]
const outs = M.outages(hist, T0, true)
is("outages newest first, each ended by what ended it",
   outs.map(o => [o.how, o.durationMs]), [["ongoing", 60000], ["shutdown", 180000], ["power", 100000]])
is("an outage whose end was never seen has no duration",
   M.outages([{ t: T0, key: "onbatt" }], T0 + 5000, false)[0], { start: T0, end: null, how: "unknown", durationMs: null })

// ---------------------------------------------------------------- words

console.log("words")
is("durations", [M.duration(45000), M.duration(192000), M.duration(3840000), M.duration(null)],
   ["45s", "3m 12s", "1h 04m", "—"])
is("when: today / yesterday / older", [
  M.when(T0, T0 + 3600000), M.when(T0 - 20 * 3600000, T0), M.when(T0 - 3 * 86400000, T0)],
  ["today 04:02", "yesterday 08:02", "Sat 26 Sep 04:02"])
is("volts trims .0", [M.volts(122.0), M.volts(26.8), M.volts(null)], ["122 V", "26.8 V", "—"])
is("battery glyphs", [100, 57, 12, 3, null].map(c => cp(M.batteryGlyph(c))),
   ["F0079", "F007F", "F007A", "F008E", "F0091"])
is("bar glyph: mains / battery / low / no data", [
  cp(M.barGlyph(ol, prot)), cp(M.barGlyph(ob, prot)), cp(M.barGlyph(oblb, prot)), cp(M.barGlyph(down, prot))],
  ["F06A5", "F007F", "F0083", "F06A6"])
is("bar label: silent on mains by default", M.barLabel(ol, prot, null, "Icon only", false), "")
is("bar label: load / input modes", [M.barLabel(ol, prot, null, "Load", false),
   M.barLabel(ol, prot, null, "Input voltage", false)], ["18%", "122V"])
is("bar label: countdown on battery", M.barLabel(ob, prot, cd, "Load", false), "2:41")
is("bar label: going down", M.barLabel(ob, prot, cd, "Icon only", true), "shutdown")
is("bar label: unprotected beats the mode", M.barLabel(ol, { ok: false, known: true }, null, "Load", false), "unprotected")
is("titles", [M.title(ol, null, false), M.title(ob, cd, false), M.title(oblb, null, false), M.title(down, null, false)],
   ["On mains", "On battery · 2:41", "Battery low", "UPS not answering"])
const monitorDown = { ok: false, known: true, reason: "the shutdown monitor is failed" }
is("hero pill is one or two words", [M.heroPill(prot), M.heroPill(monitorDown), M.heroPill(null)],
   ["protected", "NOT protected", "checking"])
is("hero meta says the one thing that matters", [
   M.heroMeta(ol, prot, pol, null, false), M.heroMeta(ob, prot, pol, cd, false),
   M.heroMeta(ob, prot, pol, cd, true), M.heroMeta(ol, monitorDown, pol, null, false),
   M.heroMeta(down, prot, pol, null, false), M.heroMeta(ol, prot, { known: true, timerSec: null }, null, false)],
   ["shuts down after 3:00 on battery", "clean shutdown at 04:05:00", "powering off now",
    "the shutdown monitor is failed", "Driver not connected", "shuts down at low battery only"])
// PanelHero upper-cases, letter-spaces and elides the meta after ~36 characters
// (measured on the 2026-09-29 screenshot); the common lines must fit whole.
is("hero meta lines fit before PanelHero elides them", [
   M.heroMeta(ol, prot, pol, null, false), M.heroMeta(ob, prot, pol, cd, false),
   M.heroMeta(ol, monitorDown, pol, null, false)].every(t => t.length <= 34), true)
is("tooltip", M.tooltip(ol, prot, null), "On mains · 122 V in · load 18% · protected")
is("tiles on battery flag the mains tile", M.tiles(ob).map(t => [t.label, t.value, t.alert]),
   [["Load", "18%", false], ["Battery", "57%", false], ["Mains", "off", true], ["UPS temp", "20.8 °C", false]])
is("detail rows", M.detailRows(ol).map(r => r.value).slice(0, 3),
   ["122 V · 60 Hz  (nominal 110 V)", "122 V", "26.8 V · 24 V nominal · low 20.8 V"])
is("service rows flag only real problems",
   M.serviceRows(M.parseServices(PROBE_OK.replace("ups-notify=active", "ups-notify=inactive"))).map(r => r.alert),
   [false, false, false, false])
is("event rows newest first", M.eventRows(hist, T0, 2).map(r => r.text), ["On battery", "Clean shutdown"])
is("pipeline tests are not outages", M.eventRows([{ t: T0, key: "test" }, { t: T0 + 1, key: "test" }], T0, 6), [])
is("last outage line", M.lastOutageText(M.outages(hist.slice(0, 2), T0, false), T0),
   "today 02:02 · 1m 40s · power came back")
is("no outages yet", M.lastOutageText([], T0), "none logged yet")

console.log("controls")
is("controls need the credential", [M.canControl(ol, svc), M.canControl(ol, { ...svc, cred: true }),
   M.canControl(down, { ...svc, cred: true })], [false, true, false])
is("setup hint only when it would help", [
   M.controlHint(ol, svc, "/p") !== "", M.controlHint(ol, { ...svc, cred: true }, "/p"), M.controlHint(down, svc, "/p")],
   [true, "", ""])
is("instcmd OK", M.instcmdResult("OK\n", "Beeper muted"), { ok: true, text: "Beeper muted" })
is("instcmd OK TRACKING", M.instcmdResult("OK TRACKING 1b2c", "x").ok, true)
is("instcmd upsd refusal is worded", M.instcmdResult("ERR ACCESS-DENIED", "Beeper").text,
   "Beeper failed: access denied (re-run the widget setup)")
is("instcmd helper refusal is worded", M.instcmdResult("ERR no-credential", "Test").text,
   "Test failed: needs the one-time widget setup")
is("instcmd silence is a failure", M.instcmdResult("", "Test").ok, false)

console.log("\n" + pass + " passed, " + fails.length + " failed")
if (fails.length) { console.log("failed: " + fails.join(", ")); process.exit(1) }
