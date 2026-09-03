import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Ui
import "Model.js" as Model

// Shipwright in the Omarchy bar: a sailboat that reports whether the git
// auto-save fleet is settled, and a popup that says which repo is not and why.
//
// Ported on 2026-08-17 from the waybar custom module the quattro upgrade
// orphaned. The waybar version could only ever be an icon plus a monospace
// tooltip, because that is all waybar's JSON contract carries. Here the same
// `shipwright health --json` payload drives a real panel: hero, info pairs,
// per-repo rows with their own cursor and tooltip, and the notes list -- the
// same vocabulary the audio, network, and dropbox panels use, so it reads as
// part of the bar rather than a guest in it.
Panel {
  id: root
  moduleName: "kyonax.shipwright"
  ipcTarget: "kyonax.shipwright"
  manageIpc: false

  property int repoIndex: 0
  property bool cursorActive: false

  // FOUR PAGES, ONE PLUGIN.
  //
  // The popup used to be one scrolling column holding everything, which meant
  // the question you opened it to answer was always somewhere below the answer
  // to a different one. Chips at the top, keys 1-4 to switch — and switching by
  // NUMBER is what frees `h` and `l`, which were bound to two unrelated TUI
  // launches while the panel's own cursor also wanted them (D14).
  // THREE TABS. Activity and attention are things you want to see without
  // asking for them, so they live at the top of the Fleet tab; a pull request
  // list is a different job with different rows and its own filter, so it keeps
  // one. Work is the third: not a list of things that are true, but a list of
  // things happening and things you can act on, with a button on every row.
  readonly property var pageNames: ["fleet", "prs", "work"]
  property int page: 0

  function setPage(index) {
    var next = Math.max(0, Math.min(pageNames.length - 1, index))
    if (next === page) return
    page = next
    cursorActive = false
    repoIndex = 0
    if (panelFlick) panelFlick.contentY = 0
  }

  // The rows the cursor walks depend on the page, so the cursor has to ask
  // rather than assume the fleet list.
  // The Fleet tab has TWO clickable bands, and j/k walks them as one list in
  // the order they are drawn: what needs you first, then the fleet.
  // The attention band is drawn GROUPED by cause (Model.attentionGroups), so
  // the rows on screen are not the CLI's items one-for-one. Everything that
  // walks, scrolls or activates a row reads this list, because a cursor over
  // the grouped rows and a cursor over the ungrouped items would disagree
  // about what row 4 is.
  property var expandedCauses: []

  readonly property var attentionGroups:
    Model.attentionGroups(shipwright.attentionItems)

  readonly property var attentionRows:
    Model.attentionRows(attentionGroups, expandedCauses)

  // The Work tab's single list. Flat, with a groupLabel on the first row of
  // each section, so the cursor walks one array and metrics() can still prove
  // dup=0 across it.
  // The repos come from the health the fleet tab already has, so a "ready to
  // run" row costs no extra process and no fetch.
  readonly property var workRows: Model.workRows(shipwright.work, shipwright.repos)

  // The chip counts what is WAITING ON YOU, not what is happening. Counting a
  // running batch would make the number go up when work starts, which is
  // backwards for a badge whose whole job is "how much is on your plate".
  readonly property int workWaiting: Model.workWaitingCount(shipwright.work)

  // The git state of a repo the attention band names. The band lists repos by
  // name; the fleet rows below hold what those repos actually look like.
  function repoDetailFor(name) {
    if (!name) return ""
    for (var i = 0; i < shipwright.repos.length; i++)
      if (shipwright.repos[i].name === name)
        return root.repoDetail(shipwright.repos[i])
    return ""
  }

  function toggleCause(key) {
    if (!key) return
    var next = []
    var found = false
    for (var i = 0; i < expandedCauses.length; i++) {
      if (String(expandedCauses[i]) === String(key)) { found = true; continue }
      next.push(expandedCauses[i])
    }
    if (!found) next.push(String(key))
    expandedCauses = next
  }

  function cursorList() {
    if (page === 0) return root.attentionRows.concat(shipwright.repos)
    if (page === 1) return shipwright.prsVisible
    if (page === 2) return root.workRows
    return []
  }

  function persistSettings(values) {
    var entry = { id: root.moduleName }
    for (var existing in root.settings) if (existing !== "id") entry[existing] = root.settings[existing]
    for (var key in values) entry[key] = values[key]
    root.settings = entry
    if (root.hostWidget && "settings" in root.hostWidget) root.hostWidget.settings = entry
    if (root.bar && root.bar.shell && typeof root.bar.shell.updateEntryInline === "function")
      root.bar.shell.updateEntryInline(root.moduleName, entry)
  }

  function setActivityWeeks(weeks) {
    shipwright.setActivityWeeks(weeks)
    persistSettings({ activityWeeks: shipwright.activityWeeks })
  }

  readonly property color foreground: bar ? bar.foreground : Color.foreground
  readonly property color urgent: bar ? bar.urgent : Color.urgent
  // NOT Qt.darker(foreground, 1.55). On this theme that computes to #a5a5a5
  // and `urgent` is #a4a4a4 — ONE unit apart, so secondary text and urgent
  // text render as the same grey and severity carried in colour is invisible.
  // Color.muted is the theme's own secondary role (#7a7a7a here), which leaves
  // a real three-step ramp: #ffffff, #8d8d8d accent, #7a7a7a muted.
  readonly property color dim: Color.muted
  readonly property string fontFamily: bar ? bar.fontFamily : Style.font.family

  // The line under a repo name. A switched-off repo says WHY in words, and a
  // repo on a streak says how long it has been owed — a number the row can
  // carry and the dot cannot.
  // The CLI's own words first. This used to rebuild the sentence here —
  // "contract changed — shipwright approve <name>" — and hand it to a column
  // about 90 px wide, which truncated it mid-word ("contract changed —
  // shipwri…") after the NEEDS YOU band above and the OUTCOME column beside it
  // had each already said the same thing. Three statements, none of them
  // finished. The reason says WHY, the outcome column says WHAT HAPPENED, and
  // the attention row says WHAT TO RUN; this column only owes the first.
  //
  // The fallbacks below carry no command either, and exist for a shipwright too
  // old to send a reason at all.
  // The GIT STATE, which is the one thing about a repo no other band shows.
  //
  // This column has now been wrong in both directions. It used to rebuild the
  // CLI's advice with the command inside it, in 90 px ("contract changed —
  // shipwri…"); then it showed the CLI's reason, which is the same sentence the
  // NEEDS YOU band above it already prints once for the whole group. Either way
  // the row spent its only free column repeating its neighbour.
  //
  // A repo's branch, and how far it has drifted from its remote, is not said
  // anywhere else on this tab — and it is what tells you whether a repo that
  // needs approval is also sitting on work you have not pushed.
  function repoDetail(repo) {
    if (!repo) return ""
    if (repo.missing) return "path does not exist"
    if (!repo.armed) return "dry run · " + Model.repoDetailText(repo)
    return Model.repoDetailText(repo)
  }

  // A repo row draws its tier header when it is the FIRST repo of that tier.
  // Computed here rather than by nesting columns, because the cursor walks one
  // flat list of children and a nested structure would make j/k land on the
  // wrong row.
  function groupLabelFor(index) {
    var repos = shipwright.repos
    if (index < 0 || index >= repos.length) return ""
    var tier = String(repos[index].tier || "")
    if (index > 0 && String(repos[index - 1].tier || "") === tier) return ""
    return tier !== "" ? (tier.charAt(0).toUpperCase() + tier.slice(1)) : "Repositories"
  }

  function groupSummaryFor(index) {
    if (groupLabelFor(index) === "") return ""
    var tier = String(shipwright.repos[index].tier || "")
    var groups = shipwright.tierGroups
    for (var i = 0; i < groups.length; i++) if (groups[i].tier === tier) return groups[i].summary
    return ""
  }

  // The rail's fill, re-evaluated by a timer while the popup is open. Bound to
  // a property rather than computed inline so the timer has one thing to poke.
  property real slotFill: 0
  // A vertical bar has no room for words, so it keeps the glyph and lets the
  // badge carry the count.
  // The glyph, and only the glyph. "11 need you" spelled out on the bar what
  // the badge already says as a number and the popup says in full — three
  // statements of one fact, the widest of them permanently occupying a status
  // bar. The count lives in the badge; the sentence lives in the tooltip.
  readonly property string barText: "󰻈"

  // What the bar says before you click it. The fleet in one line, then when
  // the next slot is — the two questions the glyph provokes.
  readonly property string barTooltipText: {
    if (!shipwright.available) return "shipwright is not installed"
    if (shipwright.lastError !== "") return shipwright.lastError
    var lines = [shipwright.fleetSummaryText]
    if (shipwright.slotDetailText !== "") lines.push(shipwright.slotDetailText)
    // By CAUSE, like the panel. Three lines of "<repo> — shipwright approve
    // <that same repo>" told you a command you could already guess and hid the
    // one thing the tooltip has room to say: how many repos are waiting on it.
    for (var i = 0; i < root.attentionGroups.length && i < 3; i++) {
      var g = root.attentionGroups[i]
      var who = g.count > 1 ? g.count + " repos" : g.repos[0]
      lines.push(who + " — " + (g.action !== "" ? g.action : g.cause))
    }
    return lines.join("\n")
  }

  // One glyph, three readings. `alert` is the only state that earns the urgent
  // colour -- pending work is normal for a fleet that is doing its job.
  readonly property color stateColor: {
    if (!shipwright.available || shipwright.lastError !== "") return urgent
    if (shipwright.fleetState === "alert") return urgent
    return foreground
  }
  // The bar glyph is coloured the way every other widget colours itself: hand
  // `active` to BarIconButton and let it reach for the theme's own active
  // colour, rather than painting a hardcoded red over the user's palette.
  readonly property bool barAlert: !shipwright.available || shipwright.lastError !== "" || shipwright.fleetState === "alert"
  // Settled is deliberately dimmer than pending -- a quiet fleet should not
  // pull the eye, the way a paused Dropbox does not.
  // Dim only when there is nothing to report. The old rule darkened everything
  // that was not `pending`, which included `alert` — so the one state that
  // wants looking at was drawn fainter than the one that does not.
  readonly property color barIconForeground:
    shipwright.fleetState === "settled" && shipwright.attentionCount === 0
      ? Qt.darker(barForeground, 1.45)
      : barForeground

  function stateColorFor(state) {
    if (state === "alert") return urgent
    if (state === "pending") return foreground
    return dim
  }

  function ensureCursor() {
    var list = cursorList()
    if (list.length === 0) { repoIndex = 0; return }
    if (repoIndex >= list.length) repoIndex = list.length - 1
    if (repoIndex < 0) repoIndex = 0
  }

  function moveCursor(dx, dy) {
    cursorActive = true
    ensureCursor()
    var list = cursorList()
    if (dy === 0 || list.length === 0) return
    repoIndex = Math.max(0, Math.min(list.length - 1, repoIndex + dy))
    scrollCursorIntoView()
  }

  function setRepoCursor(index) {
    cursorActive = true
    repoIndex = index
    scrollCursorIntoView()
  }

  function selectedRepo() {
    var list = cursorList()
    if (list.length === 0) return null
    return list[Math.max(0, Math.min(repoIndex, list.length - 1))]
  }

  // Enter opens whatever the row IS: a repo's ledger, a pull request in a
  // browser, an attention item's logs. One key, one obvious consequence per
  // page.
  function activateCursor() {
    var item = selectedRepo()
    if (!item) return
    if (page === 1) { shipwright.openUrl(item.url); return }
    // A Work row answers for itself: it already knows which of its buttons is
    // the primary one. Without this the row fell through to the fleet path
    // below, found neither `name` nor `target`, and returned — so the key hint
    // promised "⏎ act" and Enter did nothing at all.
    if (page === 2) {
      var wrow = cursorItemAt(repoIndex)
      if (wrow && wrow.activate) wrow.activate()
      return
    }
    // A folded group has no single repo to answer for; ⏎ unfolds it into the
    // repos it stands for, which is the only useful thing to do with it.
    if (item.kind === "more") { root.toggleCause(item.groupKey); return }
    // `+` binds tighter than `||`, so the name has to be picked BEFORE it is
    // concatenated: a fleet row carries `name`, an attention row `target`.
    var name = item.name !== undefined ? item.name : item.target
    if (!name) return
    // The REPORT, not the ledger. The ledger says what shipwright published;
    // the question a row in this panel provokes is why it did not. `l` still
    // opens the ledger for the times that is the question.
    shipwright.openReport(name)
  }

  // `e` — open the selected repository where the work happens. A failed
  // `npm test` is not read, it is edited, so this is the action an attention
  // row actually provokes.
  function openSelectedInEditor() {
    var item = selectedRepo()
    if (!item) return
    // A folded group stands for several repos and names none of them: unfold
    // it instead of opening an arbitrary member.
    if (item.kind === "more") { root.toggleCause(item.groupKey); return }
    var path = item.path
    if (!path) {
      // An attention row carries a repo name, not a path; find its fleet row.
      var name = item.target !== undefined && item.target !== "" ? item.target : item.name
      for (var i = 0; i < shipwright.repos.length; i++)
        if (shipwright.repos[i].name === name) { path = shipwright.repos[i].path; break }
    }
    shipwright.openEditor(path)
  }

  function scrollItemIntoView(item) {
    if (!panelFlick || !item) return
    Qt.callLater(function() {
      if (!item) return
      var margin = Style.space(6)
      var point = item.mapToItem(panelFlick.contentItem, 0, 0)
      var top = point.y
      var bottom = top + item.height
      var viewTop = panelFlick.contentY
      var viewBottom = viewTop + panelFlick.height
      var maxY = Math.max(0, panelFlick.contentHeight - panelFlick.height)
      if (top < viewTop + margin) panelFlick.contentY = Math.max(0, top - margin)
      else if (bottom > viewBottom - margin) panelFlick.contentY = Math.min(maxY, bottom + margin - panelFlick.height)
    })
  }

  // Whichever column the current page is showing. Each is a plain Column of
  // Repeater children, so the cursor index is the child index.
  // The item the cursor is on, wherever it lives. On the Fleet tab the index
  // spans TWO columns — attention first, then the fleet — so a single column
  // lookup would scroll to the wrong row for every repo.
  //
  // A Repeater inside a Column is ITSELF children[0]; its delegates follow at
  // children[1..n]. metrics() reported 11 children for ten repos, which is how
  // that surfaced — every scroll target was off by one, and had been since the
  // widget was written. The offset is computed rather than hard-coded so a
  // column that gains another child still works.
  function cursorItemAt(index) {
    var cols = page === 2 ? [workColumn]
             : page === 1 ? [prColumn]
             : [attentionColumn, fleetColumn]
    for (var c = 0; c < cols.length; c++) {
      if (!cols[c]) continue
      var kids = cols[c].children
      for (var i = 0; i < kids.length; i++)
        if (kids[i].rowIndex === index) return kids[i]
    }
    return null
  }

  // The n-th ROW of a column, skipping whatever else lives there.
  //
  // This used to be arithmetic: subtract `children.length - modelLength` from
  // the front, on the stated grounds that "a Repeater inside a Column is ITSELF
  // children[0]". Measured on the running bar, it is children[LAST] — so the
  // offset was applied at the wrong end and every lookup returned the row AFTER
  // the one asked for. That is why the header probe reported a 20 px row with no
  // tier label for a delegate that is 40 px and says "Always".
  //
  // Where the Repeater lands is not something this file should depend on either
  // way. A row is a child that has a `rowIndex`; count those.
  function rowDelegateAt(col, n) {
    if (!col || n < 0) return null
    var seen = 0
    for (var i = 0; i < col.children.length; i++) {
      if (col.children[i].rowIndex === undefined) continue
      if (seen === n) return col.children[i]
      seen += 1
    }
    return null
  }

  function scrollCursorIntoView() {
    var item = cursorItemAt(repoIndex)
    if (item) scrollItemIntoView(item)
  }

  implicitWidth: button.implicitWidth
  implicitHeight: button.implicitHeight

  // Opening always costs a local re-read (`--no-fetch`, instant) so the table is
  // never older than the click that revealed it, and additionally starts a
  // fetch when the remote-derived columns have gone stale.
  //
  // This used to call refreshQuick() directly, which shared one Process — and
  // therefore one `if (running) return` — with the ~26s fetch on the timer. The
  // click was silently dropped for the whole of that window, so the promise in
  // the line above was false exactly when it mattered: right after login, and
  // once every five minutes.
  onOpenedChanged: if (opened) {
    cursorActive = false
    if (panelFlick) panelFlick.contentY = 0
    shipwright.refreshOnOpen()
    root.updateSlotFill()
    // The strip is on the main tab now, so it loads on every open rather than
    // when a page is switched to. It is a cache read; it costs nothing.
    shipwright.refreshActivity(false)
    // Same reasoning as the strip: a cache read costs nothing, and a tab that
    // only loads when you switch to it shows a stale badge on its own chip.
    shipwright.refreshWork()
    Qt.callLater(function() { keyCatcher.forceActiveFocus() })
  }

  function updateSlotFill() {
    slotFill = Model.slotProgress(shipwright.run, shipwright.nextSlot, Date.now())
  }

  Component.onCompleted: {
    shipwright.activityWeeks = Math.max(1, Math.min(53,
      parseInt(String(root.setting("activityWeeks", 26)), 10) || 26))
  }

  // Only while the popup is open. A rail nobody is looking at is not worth a
  // timer, and the whole point of the two-process work was that this widget
  // stops doing expensive things in the background.
  Timer {
    interval: 1000
    running: root.opened
    repeat: true
    onTriggered: { root.updateSlotFill(); shipwright.tick = shipwright.tick + 1 }
  }
  onRepoIndexChanged: scrollCursorIntoView()

  Service {
    id: shipwright
    settings: root.settings
  }

  IpcHandler {
    target: root.ipcTarget
    function open(): void { root.open() }
    function close(): void { root.close() }
    function show(): void { root.open() }
    function hide(): void { root.close() }
    function toggle(): void { root.toggle() }
    function refresh(): string { shipwright.refresh(); return "ok" }
    function state(): string { return shipwright.fleetState }
    function diag(): string {
      return "opened=" + root.opened
        + " page=" + root.pageNames[root.page]
        + " quickRunning=" + shipwright.quickRunning
        + " fullRunning=" + shipwright.busy
        + " appliedSeq=" + shipwright.appliedSeq
        + " seq=" + shipwright.seq
        + " activitySeq=" + shipwright.activitySeq
        + " activityApplied=" + shipwright.activityAppliedSeq
        + " attention=" + shipwright.attentionCount
        + " repos=" + shipwright.repos.length
        + " err='" + shipwright.lastError + "'"
        + " ghOk=" + (shipwright.activity && shipwright.activity.source
                      && shipwright.activity.source.github
                      ? shipwright.activity.source.github.ok : "none")
        // The Work tab, in the only form that counts as evidence. Without these
        // an empty tab is indistinguishable from a tab whose Process never ran.
        + " workSeq=" + shipwright.workSeq
        + " workApplied=" + shipwright.workAppliedSeq
        + " workLoaded=" + shipwright.workLoaded
        + " workRows=" + root.workRows.length
        + " workWaiting=" + root.workWaiting
        + " workErr='" + shipwright.workError + "'"
    }
    // What the SHELL actually lays out, as opposed to what arithmetic about
    // the tokens predicts. Every number a design is drawn to has to come from
    // here: a row is not 20 px because the mockup says so, it is whatever
    // CursorSurface plus rowPaddingX plus a 12 px Text really measures.
    // What the SHELL actually lays out, as opposed to what arithmetic about
    // the tokens predicts. Every number a design is drawn to comes from here:
    // a row is not 20 px because a mockup says so, it is whatever
    // CursorSurface plus its padding plus a 12 px Text really measures.
    //
    // This is a measuring instrument, not decoration. It caught four things a
    // drawing could not: the content column is 368 wide and not 372, the
    // chips are content-sized and do not stretch, a two-line row is 65 px, and
    // the panel's computed `dim` lands one unit away from `urgent`.
    function metrics(): string {
      var out = []
      out.push("panelW=" + Math.round(panel.contentWidth)
             + " panelH=" + Math.round(panel.contentHeight)
             + " viewport=" + Math.round(panelFlick.height))
      out.push("colW=" + Math.round(column.width)
             + " colH=" + Math.round(column.implicitHeight))
      out.push("heroH=" + Math.round(hero.implicitHeight)
             + " chipsW=" + Math.round(pageChips.implicitWidth)
             + " chipsH=" + Math.round(pageChips.implicitHeight))
      // Band by band, because a single colH tells you it does not fit and
      // nothing about which band is why.
      var bands = []
      for (var i = 0; i < column.children.length; i++) {
        var c = column.children[i]
        if (!c.visible || c.height <= 0) continue
        bands.push(Math.round(c.height))
      }
      out.push("bands=" + bands.join(",") + " sum=" + bands.reduce(function(a,b){return a+b}, 0)
             + " spacing=" + column.spacing)
      if (fleetColumn) {
        var withHdr = null
        var plain = null
        for (var d = 0; d < shipwright.repos.length; d++) {
          var cand = root.rowDelegateAt(fleetColumn, d)
          if (!cand) continue
          if (!withHdr && cand.groupLabel !== "") withHdr = cand
          if (!plain && cand.groupLabel === "") plain = cand
        }
        out.push("rowWithHeader=" + (withHdr ? Math.round(withHdr.implicitHeight) : -1)
               + " rowPlain=" + (plain ? Math.round(plain.implicitHeight) : -1)
               + " repos=" + shipwright.repos.length)
      }
      // The Fleet tab band by band — the fold is decided here, not by colH.
      for (var q = 0; q < column.children.length; q++) {
        var t = column.children[q]
        if (!t.visible || t.height <= 0) continue
        if (t.children.length < 5) continue
        var inner = []
        for (var z = 0; z < t.children.length; z++)
          if (t.children[z].visible && t.children[z].height > 0)
            inner.push(Math.round(t.children[z].height))
        if (inner.length > 0) out.push("fleetTab=" + inner.join(",") + " total=" + Math.round(t.height))
      }
      if (attentionColumn && attentionColumn.children.length > 1)
        out.push("attRow=" + Math.round(attentionColumn.children[1].implicitHeight)
               + " attDrawn=" + (attentionColumn.children.length - 1)
               + " attRows=" + root.attentionRows.length
               + " attKinds=" + root.attentionRows.map(function (r) {
                   return r.kind.charAt(0) + r.count }).join(","))

      // Every navigable row on this page must own a UNIQUE cursor index.
      // `hasCursor` compares `root.repoIndex === rowIndex`, so two rows sharing
      // an index both light up — which is what hovering `brain` in the fleet and
      // watching "9 repos" highlight in NEEDS YOU actually was. A duplicate here
      // is that bug, and `dup=0` is the only proof the two bands are one list.
      var seen = {}
      var dup = 0
      var lo = -1
      var hi = -1
      // The columns of the page ACTUALLY SHOWN — the same set cursorItemAt
      // walks. Scanning the fleet's columns while the PR tab is up reported a
      // uniqueness result for rows nobody could hover.
      var cols = root.page === 2 ? [workColumn]
               : root.page === 1 ? [prColumn]
               : [attentionColumn, fleetColumn]
      for (var c = 0; c < cols.length; c++) {
        if (!cols[c]) continue
        var kids = cols[c].children
        for (var k = 0; k < kids.length; k++) {
          if (kids[k].rowIndex === undefined) continue
          var ix = kids[k].rowIndex
          if (seen[ix] === true) dup += 1
          seen[ix] = true
          if (lo < 0 || ix < lo) lo = ix
          if (ix > hi) hi = ix
        }
      }
      out.push("cursor: rows=" + Object.keys(seen).length
             + " dup=" + dup + " range=" + lo + ".." + hi
             + " list=" + root.cursorList().length)
      out.push("tokens: rowPaddingX=" + Style.spacing.rowPaddingX
             + " controlH=" + Style.spacing.controlHeight
             + " popupPadding=" + Style.spacing.popupPadding
             + " md=" + Style.spacing.md
             + " radius=" + Style.cornerRadius)
      out.push("font: body=" + Style.font.body + " caption=" + Style.font.caption
             + " bodySmall=" + Style.font.bodySmall + " title=" + Style.font.title
             + " family=" + Style.resolvedFontFamily)
      out.push("color: fg=" + root.foreground + " secondary=" + root.dim
             + " urgent=" + root.urgent + " accent=" + Color.accent
             + " muted=" + Color.muted)
      // The bar item is the half of this widget that is always on screen and
      // the half nothing else can measure: it lives outside the popup, so the
      // only way to know the label is not being clipped is to ask it.
      out.push("bar: size=" + (root.bar ? root.bar.barSize : -1)
             + " statusSlot=" + Style.bar.statusSlot
             + " iconFont=" + Style.bar.iconFont
             + " slot=" + Math.round(button.slotSize)
             + " textW=" + Math.round(barTextMetrics.width)
             + " badgeW=" + (barBadge.visible ? Math.round(barBadge.width) : 0)
             + " painted=" + Math.round(button.glyphPaintedWidth)
             + " text='" + root.barText + "'")
      return out.join("\n")
    }

    // Lets the A/B check drive the pages over IPC instead of by hand — the
    // widget's own report is the only evidence that a keypress did anything,
    // because the plugin lives behind a symlink onto an external disk and
    // Quickshell does not always hot-reload it.
    function page(name: string): string {
      var i = root.pageNames.indexOf(String(name))
      if (i < 0) return "unknown page"
      root.setPage(i)
      return root.pageNames[root.page]
    }
  }

  BarIconButton {
    id: button
    anchors.fill: parent
    bar: root.bar
    // Glyph and label are ONE string, the way tempo-hours does it —
    // BarIconButton keeps `labelVisible: false` and paints `text` through the
    // optical glyph canvas, so a separate label property would render nothing.
    text: root.barText
    active: root.barAlert
    foreground: root.barIconForeground
    // WidgetButton paints `activeColor` instead of `foreground` whenever
    // `active` is set, and that default is bar.urgent — #a4a4a4 in this theme.
    // So the one state that wants looking at was drawn DIMMER than every other
    // icon on the bar. The badge carries the alarm now; the ship just needs to
    // be as legible as its neighbours.
    activeColor: root.foreground
    // The glyph alone fits the status slot; the glyph plus "11 need you" does
    // not. This was a character count times a guessed advance ratio, and it was
    // guessed against the wrong string (the label, not the glyph+label actually
    // painted) at the wrong size (body-small, not the bar's icon font): it
    // returned 118 for a string that paints 101 wide under a 16 px badge.
    // BarIconButton centres `text` in `slotSize` and never clips it, so the
    // shortfall did not truncate the label, it pushed it out of the slot at
    // both ends and under the badge — which is what "11 need yo" plus a stray
    // glyph actually was. Measure the string that gets painted, at the size it
    // is painted at; `painted=` in metrics() is the renderer's own answer and
    // must equal `textW=`.
    // The status slot, widened just enough to hang a badge off the corner.
    //
    // At a bare 21 the badge had nowhere to go: BarIconButton centres the glyph
    // in the slot, so a badge anchored to the slot's top-right landed ON the
    // glyph, and its knockout ring erased what little was left. The widget
    // rendered as a grey box with a number in it and no ship at all.
    //
    // Eight more pixels move the glyph's centre right by four and give the badge
    // its own corner, so they overlap the way an icon and its notification count
    // overlap and not the way a sticker covers a photograph. The slot is only
    // wider while there is something to count.
    slotSize: shipwright.attentionCount > 0
      ? Style.bar.statusSlot + Style.space(11)
      : Style.bar.statusSlot

    TextMetrics {
      id: barTextMetrics
      font.family: button.fontFamily
      font.pixelSize: Style.bar.iconFont
      text: root.barText
    }
    tooltipText: root.barTooltipText

    onPressed: function(buttonCode) {
      if (buttonCode === Qt.RightButton) shipwright.refresh()
      else if (buttonCode === Qt.MiddleButton) shipwright.launchTui("shipwright-logs", "shipwright logs --list")
      else root.toggle()
    }

    // THE BADGE. A count, not a dot: "something is wrong" is a worse thing to
    // read from across a room than "two things are wrong", and the theme is
    // grayscale so the number is doing work the colour cannot. Knocked out of
    // the bar background the way tailscale's is, so it reads on any glyph.
    // THE BADGE. A notification count on the ship, the way an app icon carries
    // one: small, round, in the corner, overlapping the glyph rather than
    // replacing it.
    //
    // Round, in a panel whose corner radius is 0, because that is what makes it
    // read as a count rather than as a selected box — and the fleet's own state
    // dots are circles, so the shape is already in this design. Nine pixels of
    // type rather than the caption's ten: at ten, two digits are wider than the
    // ship they are supposed to be hanging off.
    Rectangle {
      id: barBadge
      visible: shipwright.attentionCount > 0
      anchors.right: parent.right
      anchors.top: parent.top
      // Clear of the bar's top edge: at 0 the knockout ring drew at y -1 and
      // the disc came back with its top sliced off.
      anchors.topMargin: Style.space(2)
      // Never narrower than it is tall, so a single digit is a circle and a
      // double digit is a pill instead of an oval squeezed around the text.
      width: Math.max(height, badgeText.implicitWidth + Style.space(2))
      height: badgeText.implicitHeight
      radius: height / 2
      // The brightest thing available, because that is what a count of things
      // waiting on you is for. `urgent` is #a4a4a4 in this theme, and a mid-grey
      // disc with darker digits on a black bar reads as a smudge on the sail
      // rather than as a number.
      color: root.foreground

      // A hairline of bar background around the badge, so it stays legible
      // where it crosses the glyph. One pixel: at two it erased the corner of
      // the ship as well, which is most of the mast.
      Rectangle {
        anchors.fill: parent
        anchors.margins: -1
        radius: width / 2
        color: root.bar ? root.bar.background : Color.background
        z: -1
      }

      Text {
        id: badgeText
        anchors.centerIn: parent
        text: shipwright.attentionCount
        color: root.bar ? root.bar.background : Color.background
        font.family: root.fontFamily
        font.pixelSize: 8
        font.bold: true
      }
    }
  }

  KeyboardPanel {
    id: panel
    anchorItem: button
    owner: root
    bar: root.bar
    open: root.opened
    focusTarget: keyCatcher
    contentWidth: panel.fittedContentWidth(Style.space(400))
    contentHeight: panel.fittedContentHeight(column.implicitHeight, Style.space(560))

    PanelKeyCatcher {
      id: keyCatcher
      anchors.fill: parent
      onMoveRequested: function(dx, dy) {
        if (!root.cursorActive) { root.cursorActive = true; return }
        root.moveCursor(dx, dy)
      }
      onActivateRequested: if (root.cursorActive) root.activateCursor()
      onCloseRequested: root.close()
      onTabRequested: function(direction) { root.switchPanel(direction) }
      // D14: `l` and `h` were bound to two unrelated TUI launches while the
      // panel's own cursor also wanted them for movement, so neither worked
      // predictably. Pages move on 1-4 now, which frees both — and `L` and `H`
      // (shifted, deliberate) keep the launches.
      onTextKey: function(t) {
        if (t === "1") root.setPage(0)
        else if (t === "2") root.setPage(1)
        else if (t === "3") root.setPage(2)
        else if (t === "e" || t === "E") root.openSelectedInEditor()
        else if (t === "l") {
          // The ledger: what shipwright actually published for this repo.
          var led = root.selectedRepo()
          var ln = led ? (led.name !== undefined ? led.name : led.target) : ""
          if (root.page === 0 && ln) shipwright.launchTui("shipwright-ledger",
                                       "shipwright ledger --repo " + ln)
        }
        else if (t === "t" || t === "T") {
          // The pull request template this repo uses, alongside the last body
          // shipwright actually wrote for it.
          var tpl = root.selectedRepo()
          var tn = tpl ? (tpl.name !== undefined ? tpl.name : tpl.target) : ""
          if (root.page === 0 && tn) shipwright.openTemplate(tn)
        }
        else if (t === "a" || t === "A") {
          // Costs money, so it gets a key of its own rather than sharing one
          // with something you might press by accident.
          var adv = root.selectedRepo()
          var an = adv ? (adv.name !== undefined ? adv.name : adv.target) : ""
          if (root.page === 0 && an) shipwright.advise(an)
        }
        else if (t === "r") shipwright.refresh()
        else if (t === "R") shipwright.syncActivity()
        else if (t === "L") shipwright.launchTui("shipwright-logs", "shipwright logs --list")
        else if (t === "H") shipwright.launchTui("shipwright-health-dash", "shipwright health")
        else if (t === "m" || t === "M") {
          if (root.page === 1) shipwright.prsMineOnly = !shipwright.prsMineOnly
        }
        else if (t === "o" || t === "O") {
          if (root.page === 1) shipwright.openAllPrs()
        }
        else if (t === "n" || t === "N") {
          var item = root.selectedRepo()
          if (root.page === 0 && item && item.target) shipwright.runNow(item.target, false)
          // On the Work tab, only a "ready to run" row. Every other row carries
          // a repo name too — a review thread's is the CLIENT repo — so keying
          // off `repo` alone would let `n` publish a client repository from a
          // row whose buttons deliberately do not offer it.
          else if (root.page === 2 && item && item.kind === "repo")
            shipwright.runNow(item.repo, false)
        }
      }

      // The key hints are PINNED, not scrolled. As the last child of a column
      // that runs 800 px with eleven repos in it, they sat about 700 px down —
      // a legend for keyboard navigation that you could only reach by
      // navigating. It costs 18 px of viewport and is worth every one.
      Text {
        id: keyHints
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: implicitHeight + Style.space(4)
        verticalAlignment: Text.AlignBottom
        text: root.page === 0
          ? "j/k move  \u21b5 why  e editor  t template  a advise  l ledger"
          : root.page === 1
          ? "j/k move  \u21b5 open  m mine/all  o open all"
          : "j/k move  \u21b5 act  n run  g go  p pause  s stop"
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
        elide: Text.ElideRight
      }

      Flickable {
        id: panelFlick
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.bottom: keyHints.top
        anchors.bottomMargin: Style.space(6)
        contentWidth: width
        contentHeight: column.implicitHeight
        clip: true
        boundsBehavior: Flickable.StopAtBounds
        flickableDirection: Flickable.VerticalFlick
        interactive: contentHeight > height
        ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

        Column {
          id: column
          width: panelFlick.width
          // TWELVE between sections, not eight.
          //
          // Eight was chosen when the tab was a budget problem, and it made
          // every gap on the page identical: the hero, its own progress rail,
          // the tabs, the numbers, the calendar and the fleet all sat the same
          // distance apart, so nothing grouped and nothing separated. A page
          // where every gap is equal has no sections in it, only rows.
          //
          // 12 between sections, 3 inside the hero (the rail IS the hero's
          // progress and belongs to it), 6 inside a band. Six gaps at 12 is
          // 72 px against the seven at 8 that came to 56 — 16 px more, spent
          // where it does the work.
          spacing: Style.space(12)

          // The hero and its rail are ONE unit, three pixels apart. The rail is
          // the hero's own progress through the day; at the section gap of 12 it
          // read as a separate band that happened to be three pixels tall.
          Column {
            width: parent.width
            spacing: Style.space(3)

            PanelHero {
              id: hero
              width: parent.width
              title: "Shipwright"
              meta: shipwright.loaded ? shipwright.runText : "Checking fleet…"
              // The countdown is a trailing control, NOT `detail:`. The kit lays the detail pill out inside the
              // hero's TITLE row while it centres the trailing control against
              // the title and the meta line together, so the countdown sat a few
              // pixels above the re-check button and nothing in this file could
              // line the two up. Handing the hero ONE trailing item makes them a
              // single unit with a single vertical centre, which is what they
              // looked like they were meant to be.
              foreground: root.foreground
              fontFamily: root.fontFamily
              iconComponent: Component {
                Text {
                  text: "󰻈"
                  color: root.stateColor
                  font.family: root.fontFamily
                  font.pixelSize: Style.font.display
                }
              }
              trailingControl: Component {
                RowLayout {
                  spacing: Style.space(8)

                  Rectangle {
                    Layout.alignment: Qt.AlignVCenter
                    visible: shipwright.slotDetailText !== ""
                    implicitWidth: heroPillText.implicitWidth + Style.space(10)
                    implicitHeight: heroPillText.implicitHeight + Style.space(4)
                    color: "transparent"
                    border.width: Style.normalBorderWidth
                    border.color: root.foreground
                    opacity: 0.45
                    radius: Style.cornerRadius

                    Text {
                      id: heroPillText
                      anchors.centerIn: parent
                      text: shipwright.slotDetailText
                      color: root.foreground
                      font.family: root.fontFamily
                      font.pixelSize: Style.font.body
                      font.bold: true
                    }
                  }

                  PanelActionButton {
                    Layout.alignment: Qt.AlignVCenter
                    iconText: "󰑐"
                    foreground: root.foreground
                    fontFamily: root.fontFamily
                    // Never disabled. A press while a check is in flight is now
                    // remembered and honoured when it lands, so the button that
                    // says "re-check" always does.
                    onClicked: shipwright.refresh()

                    // tooltipText, NOT a nested PanelToolTip. PanelActionButton
                    // keeps its MouseArea private and ships its own tooltip
                    // driven by this property; `parent.containsMouse` is
                    // undefined on it, so the nested version that used to live
                    // here was permanently invisible.
                    tooltipText: shipwright.busy
                      ? "checking…"
                      : "Re-check the fleet  (r)"
                  }
                }
              }
            }

            // THE SLOT RAIL.
            //
            // A clock, not a progress bar: nothing is loading, the day is simply
            // passing between one slot and the next. It is the one piece of the
            // popup that moves on its own, which is what makes "the fleet runs
            // without me" legible rather than something you have to remember.
            Item {
              visible: shipwright.loaded && shipwright.nextSlot !== null
              width: parent.width
              height: Style.space(3)

              Rectangle {
                anchors.fill: parent
                color: root.foreground
                opacity: 0.10
              }

              Rectangle {
                height: parent.height
                width: parent.width * root.slotFill
                color: shipwright.running ? root.foreground : root.stateColorFor("settled")
                opacity: shipwright.running ? 1.0 : 0.75
              }
            }
          }

          RowLayout {
            width: parent.width
            spacing: Style.space(8)

          ButtonGroup {
            id: pageChips
            options: [
              { value: "fleet", label: "1 Fleet" },
              { value: "prs",   label: shipwright.prs.length > 0
                                       ? "2 Pull requests " + shipwright.prs.length
                                       : "2 Pull requests" },
              { value: "work",  label: root.workWaiting > 0
                                       ? "3 Work " + root.workWaiting
                                       : "3 Work" }
            ]
            value: root.pageNames[root.page]
            foreground: root.foreground
            background: root.bar ? root.bar.background : Color.background
            accent: root.stateColorFor("settled")
            fontFamily: root.fontFamily
            fontSize: Style.font.caption
            focusable: false
            onChanged: function(v) { root.setPage(root.pageNames.indexOf(v)) }
          }

            // ButtonGroup is a Row of content-sized Buttons — it does not
            // stretch, and making it would mean reimplementing its state
            // tokens and keyboard handling for two chips. So the space it
            // leaves says when the fleet was last checked instead.
            //
            // The text takes that leftover space itself rather than being
            // pushed into it by a filling spacer: a spacer claims the slack
            // first and leaves an unbounded Text to be squeezed off the right
            // edge, which is how "checking…" ended up cut in half. Filling and
            // eliding, the worst case is an ellipsis instead of a clipped word.
            Text {
              text: shipwright.lastCheckedText
              color: root.dim
              font.family: root.fontFamily
              font.pixelSize: Style.font.caption
              horizontalAlignment: Text.AlignRight
              elide: Text.ElideRight
              Layout.fillWidth: true
              Layout.alignment: Qt.AlignVCenter
            }
          }

          // A failed check is a LINE, never a replacement for the panel. When
          // there is still a fleet on screen it says so plainly, because
          // "shipwright health returned nothing" over a table of eleven repos
          // otherwise reads as a claim about the repos rather than about the
          // check that just failed.
          Text {
            visible: shipwright.lastError !== ""
            width: parent.width
            text: shipwright.repos.length > 0
              ? "last check failed, showing the previous reading — " + shipwright.lastError
              : shipwright.lastError
            color: root.urgent
            font.family: root.fontFamily
            font.pixelSize: Style.font.bodySmall
            wrapMode: Text.WordWrap
          }

          // ===================================================== TAB 1: FLEET
          //
          // Reads top to bottom the way the questions are asked: did it run and
          // am I keeping it up (the numbers), have I been working (the strip),
          // is anything broken (needs you), and then the whole fleet by tier.
          Column {
            // NOT gated on lastError. Hiding the whole tab when a check fails
            // is what emptied the popup: the widget still held a complete
            // fleet, the hero went on reporting the run and the countdown, and
            // everything between them disappeared behind one line of red. A
            // check failing says nothing about the data already on screen —
            // it is reported above and the last good reading stays visible.
            visible: root.page === 0 && shipwright.loaded
            width: parent.width
            // These are sections, not rows: the numbers, the calendar, what
            // needs you, the fleet. Same 12 the outer column uses, so the
            // rhythm does not change halfway down the page.
            spacing: Style.space(12)

            StatTiles { width: parent.width }

            ActivityCalendar { width: parent.width }

            // NEEDS YOU — treatment B. One line per item: the repo, the CLI's
            // own reason, and an `open` affordance that puts you in the editor.
            // It grows with the problem, which is right: a fleet with five
            // broken repos SHOULD push the settled ones down.
            Column {
              visible: shipwright.attentionItems.length > 0
              width: parent.width
              spacing: Style.space(6)

              Row {
                width: parent.width
                spacing: Style.space(8)
                PanelSectionHeader {
                  text: "Needs you"
                  foreground: root.foreground
                  fontFamily: root.fontFamily
                }
                Item {
                  width: Math.max(0, parent.width - parent.children[0].implicitWidth
                                  - parent.children[2].implicitWidth - parent.spacing * 2)
                  height: 1
                }
                Text {
                  text: shipwright.attentionItems.length
                  color: root.dim
                  font.family: root.fontFamily
                  font.pixelSize: Style.font.caption
                }
              }

              Column {
                id: attentionColumn
                width: parent.width
                spacing: 0

                Repeater {
                  model: root.attentionRows
                  AttentionRow {
                    required property var modelData
                    required property int index
                    width: attentionColumn.width
                    item: modelData
                    rowIndex: index
                  }
                }
              }
            }

            PanelSeparator {
              visible: shipwright.repos.length > 0
              foreground: root.foreground
            }

            // The fleet, grouped by tier. One flat column so the cursor index
            // and the child index stay in step; the headers are drawn by the
            // first row of each tier.
            Column {
              id: fleetColumn
              width: parent.width
              spacing: 0

              Repeater {
                model: shipwright.repos
                RepoRow {
                  required property var modelData
                  required property int index
                  width: fleetColumn.width
                  repo: modelData
                  // The cursor walks ONE list — attention rows, then the fleet
                  // (cursorList()) — so a fleet row's cursor index is its
                  // position in THAT list, not in its own Repeater. Passing the
                  // bare Repeater index gave repo 0 and attention row 0 the
                  // same index, and `hasCursor` compares indices: hovering
                  // `brain` lit up "9 repos" in NEEDS YOU at the same time.
                  rowIndex: root.attentionRows.length + index
                  // The tier header still keys off the position in the fleet.
                  groupLabel: root.groupLabelFor(index)
                  groupSummary: root.groupSummaryFor(index)
                }
              }
            }

            Text {
              visible: shipwright.repos.length === 0
              width: parent.width
              text: "No repositories enrolled."
              color: root.dim
              font.family: root.fontFamily
              font.pixelSize: Style.font.bodySmall
            }

          }

          // =============================================== TAB 2: PULL REQUESTS
          //
          // The browser rule in 90-report.sh refuses to open a PR raised in a
          // night slot, on purpose. This page is the morning surface for it.
          Column {
            visible: root.page === 1
            width: parent.width
            spacing: Style.space(8)

            Row {
              width: parent.width
              spacing: Style.space(8)

              PanelSectionHeader {
                text: "Pull requests"
                foreground: root.foreground
                fontFamily: root.fontFamily
              }

              Text {
                anchors.verticalCenter: parent.verticalCenter
                text: shipwright.prsSummaryText
                color: shipwright.prsMine > 0 ? root.stateColorFor("alert") : root.dim
                font.family: root.fontFamily
                font.pixelSize: Style.font.caption
              }
            }

            // Said once, at the top, rather than on every row: this is the
            // consequence people do not expect from a PR flow.
            Text {
              visible: shipwright.prsMine > 0
              width: parent.width
              text: "Work on these branches is not in your working tree until you merge and pull."
              color: root.dim
              font.family: root.fontFamily
              font.pixelSize: Style.font.caption
              wrapMode: Text.WordWrap
            }

            // MINE / ALL. `mine` means shipwright raised it, which is the case
            // that matters: restore_branch already put the checkout back, so
            // that work is on its branch and not in the tree until it merges.
            ButtonGroup {
              visible: shipwright.prs.length > 0
              options: [
                { value: "mine", label: "Mine " + shipwright.prsMine },
                { value: "all",  label: "All " + shipwright.prs.length }
              ]
              value: shipwright.prsMineOnly ? "mine" : "all"
              foreground: root.foreground
              background: root.bar ? root.bar.background : Color.background
              accent: root.stateColorFor("settled")
              fontFamily: root.fontFamily
              fontSize: Style.font.caption
              focusable: false
              onChanged: function(v) { shipwright.prsMineOnly = (v === "mine") }
            }

            Column {
              id: prColumn
              width: parent.width
              spacing: Style.space(6)

              Repeater {
                model: shipwright.prsVisible
                PrRow {
                  required property var modelData
                  required property int index
                  width: prColumn.width
                  pr: modelData
                  rowIndex: index
                }
              }
            }

            Button {
              visible: shipwright.prsVisible.length > 1
              iconText: "󰖟"
              text: "Open all " + shipwright.prsVisible.length + "  (o)"
              bordered: true
              foreground: root.foreground
              accent: root.stateColorFor("settled")
              fontFamily: root.fontFamily
              fontSize: Style.font.caption
              onClicked: shipwright.openAllPrs()
            }

            Text {
              visible: shipwright.prs.length === 0
              width: parent.width
              text: "Nothing open. Everything shipwright raised has been merged."
              color: root.dim
              font.family: root.fontFamily
              font.pixelSize: Style.font.bodySmall
              wrapMode: Text.WordWrap
            }
          }

          // ========================================================= TAB 3: WORK
          //
          // Not an inbox list. The Fleet tab reports things that are TRUE; this
          // one carries things that are HAPPENING and things you can act on,
          // and every row has a button that does the obvious thing.
          Column {
            visible: root.page === 2
            width: parent.width
            spacing: Style.space(8)

            Row {
              width: parent.width
              Text {
                text: shipwright.workLoaded
                  ? (root.workWaiting === 0 ? "Nothing is waiting on you"
                                            : root.workWaiting + " waiting on you")
                  : "reading\u2026"
                color: root.foreground
                font.family: root.fontFamily
                font.pixelSize: Style.font.caption
              }
              Item {
                width: Math.max(0, parent.width - parent.children[0].implicitWidth
                                  - workAge.implicitWidth)
                height: 1
              }
              Text {
                id: workAge
                // What it is showing, and how old that is. The bar never
                // fetches, so "when was this true" is not a detail.
                text: shipwright.work && shipwright.work.checked_age_s !== undefined
                  ? "checked " + Model.humanDuration(shipwright.work.checked_age_s) + " ago"
                  : ""
                color: root.dim
                font.family: root.fontFamily
                font.pixelSize: Style.font.caption
              }
            }

            Text {
              visible: shipwright.workError !== ""
              width: parent.width
              text: shipwright.workError
              color: root.urgent
              font.family: root.fontFamily
              font.pixelSize: Style.font.bodySmall
              wrapMode: Text.WordWrap
            }

            Column {
              id: workColumn
              width: parent.width
              spacing: 0

              Repeater {
                model: root.workRows
                WorkRow {
                  width: workColumn.width
                  item: modelData
                  rowIndex: index
                }
              }
            }

            Text {
              visible: shipwright.workLoaded && root.workRows.length === 0
                       && shipwright.workError === ""
              width: parent.width
              text: "Nothing is running and nothing is blocked. Every pull request is mergeable, reviewed and green."
              color: root.dim
              font.family: root.fontFamily
              font.pixelSize: Style.font.bodySmall
              wrapMode: Text.WordWrap
            }
          }

        }
      }
    }
  }

  // ------------------------------------------------------------- components

  component PrRow: CursorSurface {
    id: prRow
    property var pr: null
    property int rowIndex: 0

    hasCursor: root.page === 1 && root.cursorActive && root.repoIndex === rowIndex

    // Shipwright's own PRs are the ones holding work off the default branch,
    // so they carry the alert colour; everyone else's are informational.
    readonly property color accentColor:
      prRow.pr && prRow.pr.mine ? root.stateColorFor("alert") : root.dim

    foreground: root.foreground
    implicitHeight: prContent.implicitHeight + Style.spacing.rowPaddingX

    MouseArea {
      id: prMouse
      anchors.fill: parent
      hoverEnabled: true
      cursorShape: Qt.PointingHandCursor
      onEntered: root.setRepoCursor(prRow.rowIndex)
      onClicked: shipwright.openUrl(prRow.pr ? prRow.pr.url : "")
    }

    PanelToolTip {
      x: Math.min(Style.space(7), Math.max(0, prRow.width - width))
      y: prRow.height + Style.space(2)
      visible: prMouse.containsMouse && Model.prTooltip(prRow.pr) !== ""
      text: Model.prTooltip(prRow.pr)
      fontFamily: root.fontFamily
    }

    RowLayout {
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.verticalCenter: parent.verticalCenter
      anchors.leftMargin: Style.space(10)
      anchors.rightMargin: Style.space(10)
      spacing: Style.space(8)

      Rectangle {
        width: Style.space(7)
        height: Style.space(7)
        radius: width / 2
        color: prRow.accentColor
        opacity: prRow.pr && prRow.pr.mine ? 1.0 : 0.55
        Layout.alignment: Qt.AlignVCenter
      }

      ColumnLayout {
        id: prContent
        Layout.fillWidth: true
        spacing: Style.space(1)

        Text {
          Layout.fillWidth: true
          text: Model.prTitleText(prRow.pr)
          color: root.foreground
          font.family: root.fontFamily
          font.pixelSize: Style.font.body
          elide: Text.ElideRight
        }

        Text {
          Layout.fillWidth: true
          text: Model.prDetailText(prRow.pr)
          color: root.dim
          font.family: root.fontFamily
          font.pixelSize: Style.font.caption
          elide: Text.ElideRight
        }
      }
    }
  }

  // An Item, NOT a CursorSurface, because the tier header is drawn by the first
  // row of its tier and a CursorSurface at the delegate's root paints its hover
  // fill over that header too — hovering `brain` boxed up "Always · 0 of 4
  // settled" along with it, which reads as the section being selected. The
  // highlight belongs to the row; the header is a label that happens to be
  // carried by it.
  //
  // The header stays inside the delegate (rather than becoming its own child of
  // fleetColumn) because the cursor walks the column's children and computes its
  // offset from `children.length - modelLength`; interleaved header items would
  // put every scroll target back off by one.
  component RepoRow: Item {
    id: repoRow
    property var repo: null
    property int rowIndex: 0
    property string groupLabel: ""
    property string groupSummary: ""

    readonly property string state: Model.repoState(repo)
    readonly property bool isOff: Model.repoIsOff(repo)
    // A switched-off repo is neither settled nor broken. It goes dim and says
    // so in words, rather than borrowing a colour that means something else.
    readonly property color accentColor:
      isOff ? root.dim : root.stateColorFor(state)

    // The gap goes ABOVE the heading, for the reason spelled out on
    // AttentionRow: a tier label that is 10 px from its own first repo and
    // flush against the last repo of the tier before it has grouped the page
    // the wrong way round.
    implicitHeight: rowBody.implicitHeight
      + (groupLabel !== ""
         ? Style.space(10) + groupHeader.implicitHeight + Style.space(2)
         : 0)

    // The tier header, drawn by the first row of each tier.
    Row {
      id: groupHeader
      visible: repoRow.groupLabel !== ""
      anchors.top: parent.top
      anchors.topMargin: Style.space(10)
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.leftMargin: Style.space(10)
      anchors.rightMargin: Style.space(10)
      spacing: Style.space(8)

      Text {
        id: groupHeaderLabel
        text: repoRow.groupLabel
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
        font.letterSpacing: 1
      }

      Item {
        width: Math.max(0, parent.width - groupHeaderLabel.implicitWidth
                          - groupHeaderSummary.implicitWidth - parent.spacing * 2)
        height: 1
        anchors.verticalCenter: parent.verticalCenter
        Rectangle {
          anchors.verticalCenter: parent.verticalCenter
          width: parent.width
          height: 1
          color: root.foreground
          opacity: 0.12
        }
      }

      Text {
        id: groupHeaderSummary
        text: repoRow.groupSummary
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }
    }

    CursorSurface {
      id: rowBody
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.bottom: parent.bottom

      // 20 px: a 12 px line of text measures 16, plus 4 px of padding.
      //
      // The kit's rowPaddingX is 12, which would make this 28 and push the last
      // three repos below the fold. That divergence is deliberate and it is the
      // only reason the whole tab fits its 528 px viewport — this panel is a
      // dense list, not a settings form.
      implicitHeight: repoName.implicitHeight + Style.space(4)
      height: implicitHeight

      hasCursor: root.page === 0 && root.cursorActive
                 && root.repoIndex === repoRow.rowIndex
      foreground: root.foreground

      MouseArea {
        id: repoMouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onEntered: root.setRepoCursor(repoRow.rowIndex)
        onClicked: root.activateCursor()
      }

      PanelToolTip {
        x: Math.min(Style.space(7), Math.max(0, rowBody.width - width))
        y: rowBody.height + Style.space(2)
        visible: repoMouse.containsMouse && Model.repoTooltip(repoRow.repo) !== ""
        text: Model.repoTooltip(repoRow.repo)
        fontFamily: root.fontFamily
      }

      RowLayout {
        anchors.fill: parent
        anchors.leftMargin: Style.space(10)
        anchors.rightMargin: Style.space(10)
        spacing: Style.space(8)

        // A dot rather than a per-state glyph: the colour already carries the
        // meaning, and ten identical shapes keep the list scannable. Needs-input
        // gets a ring as well, because the theme is grayscale and colour alone
        // cannot separate "urgent" from "normal" at a glance.
        Rectangle {
          width: Style.space(7)
          height: Style.space(7)
          radius: width / 2
          color: repoRow.state === "pending" && !repoRow.isOff ? "transparent" : repoRow.accentColor
          border.width: repoRow.state === "pending" && !repoRow.isOff ? 1 : 0
          border.color: root.foreground
          opacity: repoRow.state === "settled" && !repoRow.isOff ? 0.55 : 1.0
          Layout.alignment: Qt.AlignVCenter

          Rectangle {
            visible: repoRow.state === "alert" && !repoRow.isOff
            anchors.centerIn: parent
            width: parent.width + Style.space(4)
            height: width
            radius: width / 2
            color: "transparent"
            border.width: 1
            border.color: root.urgent
            opacity: 0.45
          }
        }

        // ONE line: name, then the git state, then the outcome. Two lines was
        // 43 px a row and 450 px for ten repos, which does not fit anything.
        Text {
          id: repoName
          Layout.preferredWidth: Style.space(118)
          text: repoRow.repo ? repoRow.repo.name : ""
          color: repoRow.isOff ? root.dim : root.foreground
          font.family: root.fontFamily
          font.pixelSize: Style.font.body
          elide: Text.ElideRight
        }

        Text {
          Layout.fillWidth: true
          text: root.repoDetail(repoRow.repo)
          color: root.dim
          font.family: root.fontFamily
          font.pixelSize: Style.font.caption
          elide: Text.ElideRight
        }

        Text {
          text: Model.repoOutcomeText(repoRow.repo)
          color: repoRow.accentColor
          // The ONLY bold text in the popup is an outcome that needs a person.
          // In a grayscale theme, weight is the contrast that hue would be.
          font.bold: repoRow.state === "alert" && !repoRow.isOff
          font.family: root.fontFamily
          font.pixelSize: Style.font.caption
          Layout.alignment: Qt.AlignVCenter
        }
      }
    }
  }

  // One card per thing only a person can clear. The reason and the action are
  // the CLI's own words (sw_outcome_reason / sw_outcome_action), never
  // re-derived here — the bar and the terminal giving different advice about
  // the same repo is exactly what the shared attention table exists to stop.
  // ------------------------------------------------- the four numbers
  //
  // Four answers in the order they are asked: did it run, did I work, am I
  // keeping it up, is anything broken. Hairline-separated rather than boxed,
  // so they read as one band.
  component StatTile: Column {
      id: tile
      property string value: ""
      property string label: ""
      property bool alert: false
      spacing: 0
      leftPadding: Style.space(7)

      Text {
        text: tile.value
        color: tile.alert ? root.urgent : root.foreground
        font.family: root.fontFamily
        font.pixelSize: Style.font.title
      }
      Text {
        text: tile.label
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
        font.letterSpacing: 0.8
      }
    }

  component StatTiles: RowLayout {
    spacing: 1

    StatTile {
      Layout.fillWidth: true
      value: shipwright.run ? String(shipwright.run.slot || "—") : "—"
      label: "Last run"
    }
    Rectangle { Layout.preferredWidth: 1; Layout.fillHeight: true
                color: root.foreground; opacity: 0.12 }
    StatTile {
      Layout.fillWidth: true
      value: shipwright.activity ? String(Model.todayCount(shipwright.activity)) : "—"
      label: "Today"
    }
    Rectangle { Layout.preferredWidth: 1; Layout.fillHeight: true
                color: root.foreground; opacity: 0.12 }
    StatTile {
      Layout.fillWidth: true
      value: shipwright.activity ? String(shipwright.activity.streak.current) + "d" : "—"
      label: "Streak"
    }
    Rectangle { Layout.preferredWidth: 1; Layout.fillHeight: true
                color: root.foreground; opacity: 0.12 }
    StatTile {
      Layout.fillWidth: true
      value: String(shipwright.attentionCount)
      alert: shipwright.attentionCount > 0
      label: "Need you"
    }
  }

  // --------------------------------------------- the activity calendar
  //
  // Seven weekday ROWS by as many week COLUMNS as the column is wide, which is
  // the GitHub profile graph. A single row of squares answered "have I been
  // working" and nothing else — you could not tell a Saturday from a Tuesday in
  // it, so two months of empty Fridays looked exactly like two months of steady
  // work. Here it is a shape, and the shape is the point.
  //
  // The squares are GITHUB's contribution counts, so they match the profile
  // page. Four steps of accent ALPHA — never a hue ramp: this theme is
  // grayscale and a green heatmap would be the only coloured thing on the bar.
  component ActivityCalendar: Column {
    id: cal
    spacing: Style.space(5)

    // 12 px cells with a 2 px gutter, and a 22 px rail for the weekday labels:
    // 24 columns then measure 359 of the 368 available, which is as close to
    // full width as whole cells get. The count is DERIVED from the width rather
    // than fixed, so a narrower bar drops columns instead of clipping them.
    readonly property int cellSize: Style.space(12)
    readonly property int cellGap: Style.space(2)
    readonly property int railW: Style.space(22)
    readonly property int gutter: Style.space(3)

    readonly property int weeksThatFit: Math.max(1, Math.floor(
      (width - railW - gutter + cellGap) / (cellSize + cellGap)))

    readonly property var grid: Model.calendarGrid(shipwright.activity, weeksThatFit)
    readonly property string caveat: Model.activityCaveatText(shipwright.activity)

    // GitHub's ramp starts at a VISIBLE floor, not at nothing: its zero-days are
    // a filled square barely above the page, and it draws no borders at all.
    // Ours drew an empty day as transparent-plus-a-hairline, and at 24 columns
    // most days are empty — so the thing you saw first was a lattice of outlines
    // with a few filled cells inside it, rather than the shape of the work.
    // A faint fill says "no contributions" just as clearly and disappears into
    // the background instead of drawing it.
    function levelColor(level) {
      var base = root.stateColorFor("settled")
      var alphas = [0.07, 0.30, 0.52, 0.76, 1.0]
      return Qt.rgba(base.r, base.g, base.b, alphas[Math.max(0, Math.min(4, level))])
    }

    Row {
      width: parent.width
      spacing: Style.space(8)
      Text {
        id: calHeader
        text: Model.calendarHeaderText(cal.grid.weeks)
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
        font.bold: true
        font.letterSpacing: 0.8
      }
      Item {
        width: Math.max(0, parent.width - calHeader.implicitWidth
                        - calSummary.implicitWidth - parent.spacing * 2)
        height: 1
      }
      Text {
        id: calSummary
        text: Model.calendarSummaryText(shipwright.activity, cal.weeksThatFit)
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }
    }

    // Month labels, over the first column that belongs to each month. Without
    // them the grid is two dozen identical squares with no way to say when.
    Row {
      spacing: cal.cellGap
      Item { width: cal.railW + cal.gutter - cal.cellGap; height: 1 }
      Repeater {
        model: cal.grid.months
        Item {
          required property var modelData
          width: cal.cellSize
          height: monthLabel.implicitHeight
          Text {
            id: monthLabel
            text: modelData
            color: root.dim
            font.family: root.fontFamily
            font.pixelSize: Style.font.caption
            // A month name is wider than one 12 px column, so it is allowed to
            // run over the columns after it rather than be clipped to a letter.
            width: implicitWidth
          }
        }
      }
    }

    Column {
      id: calBody
      spacing: cal.cellGap

      Repeater {
        model: 7
        Row {
          required property int index
          spacing: cal.cellGap

          Text {
            width: cal.railW + cal.gutter - cal.cellGap
            horizontalAlignment: Text.AlignRight
            anchors.verticalCenter: parent.verticalCenter
            text: Model.WEEKDAY_RAIL[index]
            color: root.dim
            font.family: root.fontFamily
            font.pixelSize: Style.font.caption
          }

          Repeater {
            model: cal.grid.rows[index]
            Rectangle {
              required property var modelData
              width: cal.cellSize
              height: cal.cellSize
              // A short final week leaves real holes in the last column. Those
              // are not empty days, they are days that have not happened, and
              // an outline would claim they had.
              visible: modelData !== null
              color: modelData ? cal.levelColor(modelData.level) : "transparent"
              // ONLY today is outlined now. An empty day carries the ramp's
              // floor fill instead of a hairline, which is what stops the grid
              // reading as a lattice; today keeps the full-strength outline,
              // because that one IS meant to be found.
              border.width: modelData && modelData.isToday ? 1 : 0
              border.color: root.foreground

              MouseArea { id: cellMouse; anchors.fill: parent; hoverEnabled: true }

              // An unplaced ToolTip sits ABOVE its parent, which on the top row
              // puts the day's detail over the month labels and on the last
              // column pushes it off the right edge. Below the cell, clamped
              // into the calendar's width — `parent.x` is the cell's offset
              // inside its Row, so the sum is the position in cal coordinates.
              PanelToolTip {
                readonly property real cellX: parent.x + cal.railW + cal.gutter
                x: Math.max(-cellX, Math.min(0, cal.width - width - cellX))
                y: parent.height + Style.space(3)
                visible: cellMouse.containsMouse && modelData !== null
                text: modelData ? Model.dayTooltip(modelData) : ""
                fontFamily: root.fontFamily
              }
            }
          }
        }
      }
    }

    // THE LEGEND. GitHub closes its calendar with one ("Less ▢▢▢▢▢ More") and
    // this grid had none, which mattered more here than it does there: GitHub's
    // scale is fixed across the year, while ours saturates at the 75th
    // percentile of whatever window is drawn, so the same shade means different
    // things at 12 weeks and at 52. Without the number beside it the intensity
    // was not interpretable at all — it just looked like a heatmap.
    //
    // The swatches are the grid's OWN ramp at the grid's OWN cell size, so the
    // match is something you can see rather than something you have to trust.
    Row {
      width: parent.width
      spacing: Style.space(8)

      Text {
        id: calScale
        anchors.verticalCenter: parent.verticalCenter
        text: Model.calendarScaleText(cal.grid.peak)
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }

      Item {
        width: Math.max(0, parent.width - calScale.implicitWidth
                        - calLess.implicitWidth - calRamp.implicitWidth
                        - calMore.implicitWidth - parent.spacing * 4)
        height: 1
      }

      Text {
        id: calLess
        anchors.verticalCenter: parent.verticalCenter
        text: "Less"
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }

      Row {
        id: calRamp
        anchors.verticalCenter: parent.verticalCenter
        spacing: cal.cellGap
        Repeater {
          model: 5
          Rectangle {
            required property int index
            width: cal.cellSize
            height: cal.cellSize
            color: cal.levelColor(index)
          }
        }
      }

      Text {
        id: calMore
        anchors.verticalCenter: parent.verticalCenter
        text: "More"
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }
    }

    // Only when there is something to say. A caveat that is always on screen
    // stops being read.
    Text {
      visible: cal.caveat !== ""
      width: parent.width
      text: cal.caveat
      color: root.dim
      font.family: root.fontFamily
      font.pixelSize: Style.font.caption
      elide: Text.ElideRight
    }
  }

  // -------------------------------------------------- one thing to do
  //
  // One row per repo, grouped under the COMMAND that clears it.
  //
  // The command is a heading carried by the first row of its group — the same
  // shape as a tier header in RepoRow, and for the same reason: the cursor walks
  // one flat list of rows, and a nested structure would make j/k land on a
  // heading. As there, the heading sits OUTSIDE the CursorSurface, so hovering a
  // repo never paints over the command above it.
  //
  // The row carries what the heading cannot: which repo, and the state its
  // worktree is actually in. Nine identical sentences was the previous answer
  // and it told you nothing about any of the nine.
  //
  // 20 px: a 12 px line of text plus 4 px of padding — deliberately tighter than
  // the kit's rowPaddingX, and the reason the whole tab fits 528.
  // A Work row: a heading when it starts a section, a 20 px body, a progress
  // rail under anything that is mid-batch, and buttons that do the obvious
  // thing.
  //
  // THE BUTTONS EACH GET THEIR OWN MouseArea, layered OVER the row's. A kit
  // Button is 28 px and will not fit a 20 px row, so these are the same
  // hairline pills the attention band uses. Each one must accept its click
  // without swallowing the hover that sets the cursor, which is why the row's
  // MouseArea stays hoverEnabled underneath rather than being replaced.
  component WorkRow: Item {
    id: wk
    property var item: null
    property int rowIndex: 0

    readonly property string groupLabel: wk.item ? String(wk.item.groupLabel || "") : ""
    readonly property string groupSub: wk.item ? String(wk.item.groupSub || "") : ""
    readonly property var groupButtons: wk.item && wk.item.groupButtons ? wk.item.groupButtons : []
    readonly property var buttons: wk.item && wk.item.buttons ? wk.item.buttons : []
    readonly property real progress: wk.item ? Number(wk.item.progress) : -1
    readonly property bool hasRail: wk.progress >= 0

    readonly property string tipText: {
      if (!wk.item) return ""
      var bits = []
      if (wk.item.detail !== "") bits.push(String(wk.item.detail))
      if (wk.item.state !== "") bits.push("state: " + wk.item.state)
      if (wk.item.action !== "") bits.push(String(wk.item.action))
      return bits.join("\n")
    }

    // The same rhythm the attention band uses, and for the same reason: the gap
    // that separates two sections belongs ABOVE the heading, not between the
    // heading and the rows it labels. 10 above, 2 below.
    // A rail is EXTRA CHROME BETWEEN TWO ROWS, so it needs air on both sides.
    // It had 3 above and none below, so the bar sat flush against the next
    // row's text and read as that row's underline rather than as this row's
    // progress — worst exactly where rows are densest, which is where a rail
    // appears at all.
    implicitHeight: wkBody.implicitHeight
      + (groupLabel !== "" ? Style.space(10) + wkHeader.implicitHeight + Style.space(2) : 0)
      + (hasRail ? Style.space(3) + 3 + Style.space(4) : 0)

    // THREE KINDS OF BUTTON, and getting the kind wrong is how one does nothing
    // visible:
    //   a TERMINAL for anything long or anything that publishes, so it can be
    //     watched and interrupted;
    //   a DETACHED command for one that opens its own editor window;
    //   a DETACHED command for an instant state flip, where the row changing is
    //     the feedback.
    // A command that merely PRINTS is the one shape that must never be
    // detached: its output goes nowhere and the press looks like a no-op.
    function act(which) {
      if (!wk.item) return
      var id = String(wk.item.key)

      // Instant state flips. The tab re-reads a moment later, so the row
      // changing under you is the confirmation.
      if (which === "go")      { shipwright.workAction("agent go", id); return }
      if (which === "pause")   { shipwright.workAction("agent pause", id); return }
      if (which === "stop")    { shipwright.workAction("agent stop", id); return }
      if (which === "resume")  { shipwright.workAction("resume", wk.item.repo); return }
      if (which === "approve") { shipwright.workAction("approve", wk.item.repo); return }

      // Long, and they publish. A terminal you can watch and Ctrl-C, which is
      // the same path the `n` key has always used.
      if (which === "run")     { shipwright.runNow(wk.item.repo, false); return }
      if (which === "dry")     { shipwright.runNow(wk.item.repo, true); return }
      if (which === "agent") {
        shipwright.launchTui("shipwright-agent",
                             "shipwright agent --pr " + String(wk.item.pr))
        return
      }

      if (which === "read") {
        // `read` means "show me the thing this row is about", and that is a
        // different document per kind. openReport takes a REPO and runs
        // `shipwright why`; openEditor takes a PATH. Handing one the other's
        // argument opens nothing and reports no error.
        if (wk.item.planPath !== "") { shipwright.openEditor(wk.item.planPath); return }
        // A contract you are being asked to approve: the diff has to be one
        // click away, which is the only reason an approve button on a bar is
        // acceptable at all. `contract --all` WRITES contracts.org and opens it
        // in the editor; plain `contract <repo>` only prints, so detached it
        // would show you nothing at all.
        if (wk.item.kind === "drift") { shipwright.workAction("contract --all"); return }
        if (wk.item.url !== "") { shipwright.openUrl(wk.item.url); return }
        // `shipwright inbox` opens the document itself, so it needs no terminal.
        shipwright.workAction("inbox")
      }
    }

    // The heading's buttons act on the WHOLE group, which for a pull request is
    // the whole batch: `shipwright agent --pr N` already takes every blocker on
    // that PR, so pressing it once per thread would start the same batch
    // fifteen times.
    function actGroup(which) {
      if (!wk.item) return
      if (which === "agent") {
        shipwright.launchTui("shipwright-agent",
                             "shipwright agent --pr " + String(wk.item.groupPr))
        return
      }
      if (which === "read") { shipwright.workAction("inbox") }
    }

    // Enter does whatever the row's FIRST button does. A row whose primary
    // action is unreachable from the keyboard is a row the cursor can select
    // and then do nothing with.
    function activate() { if (wk.buttons.length > 0) wk.act(String(wk.buttons[0])) }

    Row {
      id: wkHeader
      visible: wk.groupLabel !== ""
      anchors.top: parent.top
      anchors.topMargin: Style.space(10)
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.leftMargin: Style.space(7)
      spacing: Style.space(8)

      Text {
        id: wkHeaderLabel
        text: wk.groupLabel
        color: root.foreground
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
        font.bold: true
      }

      // What the heading's buttons will act on, said once for the group rather
      // than repeated down every row.
      Text {
        id: wkHeaderSub
        anchors.verticalCenter: parent.verticalCenter
        text: wk.groupSub
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }

      Item {
        width: Math.max(0, parent.width - wkHeaderLabel.implicitWidth
                          - wkHeaderSub.implicitWidth - wkGroupPills.implicitWidth
                          - wkHeaderCount.implicitWidth - parent.spacing * 4)
        height: 1
        anchors.verticalCenter: parent.verticalCenter
        Rectangle {
          anchors.verticalCenter: parent.verticalCenter
          width: parent.width
          height: 1
          color: root.foreground
          opacity: 0.25
        }
      }

      Row {
        id: wkGroupPills
        anchors.verticalCenter: parent.verticalCenter
        spacing: Style.space(4)

        Repeater {
          model: wk.groupButtons
          Rectangle {
            id: gpill
            required property var modelData
            anchors.verticalCenter: parent.verticalCenter
            width: gpillLabel.implicitWidth + Style.space(8)
            height: gpillLabel.implicitHeight + Style.space(2)
            color: "transparent"
            border.width: 1
            // Brighter than a row's pill: this one acts on everything below it,
            // and a button with a larger blast radius should not look identical
            // to the one beside it that touches a single thread.
            border.color: root.foreground
            opacity: gpillMouse.containsMouse ? 0.85 : 0.45

            Text {
              id: gpillLabel
              anchors.centerIn: parent
              text: String(gpill.modelData) + " all"
              color: root.foreground
              font.family: root.fontFamily
              font.pixelSize: Style.font.caption
            }

            MouseArea {
              id: gpillMouse
              anchors.fill: parent
              hoverEnabled: true
              cursorShape: Qt.PointingHandCursor
              onClicked: wk.actGroup(String(gpill.modelData))
            }
          }
        }
      }

      Text {
        id: wkHeaderCount
        text: wk.item && wk.item.groupCount > 0 ? String(wk.item.groupCount) : ""
        color: root.dim
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }
    }

    CursorSurface {
      id: wkBody
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.top: wk.groupLabel !== "" ? wkHeader.bottom : parent.top
      anchors.topMargin: wk.groupLabel !== "" ? Style.space(2) : 0
      implicitHeight: wkText.implicitHeight + Style.space(4)
      height: implicitHeight

      hasCursor: root.page === 2 && root.cursorActive && root.repoIndex === wk.rowIndex
      foreground: root.foreground

      MouseArea {
        id: wkMouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onEntered: root.setRepoCursor(wk.rowIndex)
        onClicked: wk.activate()
      }

      PanelToolTip {
        x: Math.min(Style.space(7), Math.max(0, wkBody.width - width))
        y: wkBody.height + Style.space(2)
        visible: wkMouse.containsMouse && wk.tipText !== ""
        text: wk.tipText
        fontFamily: root.fontFamily
      }

      Rectangle {
        anchors.left: parent.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        width: Style.space(2)
        color: wk.hasRail ? root.stateColorFor("settled") : root.dim
      }

      Row {
        id: wkRow
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        anchors.leftMargin: Style.space(7)
        spacing: Style.space(6)

        Text {
          id: wkText
          text: wk.item ? String(wk.item.label) : ""
          color: root.foreground
          font.family: root.fontFamily
          font.pixelSize: Style.font.body
        }

        Text {
          id: wkDetail
          anchors.verticalCenter: parent.verticalCenter
          width: Math.max(0, wkRow.width - wkText.implicitWidth
                            - wkPills.implicitWidth - wkRow.spacing * 2
                            - Style.space(7))
          text: {
            if (!wk.item) return ""
            // The STATE first, with the count appended only once there is one.
            // Falling back to `detail` whenever total was 0 meant the opening
            // seconds of a run showed the tooltip's whole sentence instead of
            // the two words that belong on a 20 px row.
            if (String(wk.item.state) !== "")
              return wk.item.state
                   + (wk.item.total > 0 ? " · " + wk.item.done + " of " + wk.item.total : "")
            return String(wk.item.detail)
          }
          color: root.dim
          font.family: root.fontFamily
          font.pixelSize: Style.font.caption
          elide: Text.ElideRight
        }

        Row {
          id: wkPills
          anchors.verticalCenter: parent.verticalCenter
          spacing: Style.space(4)

          Repeater {
            model: wk.buttons
            Rectangle {
              id: pill
              required property var modelData
              anchors.verticalCenter: parent.verticalCenter
              width: pillLabel.implicitWidth + Style.space(8)
              height: pillLabel.implicitHeight + Style.space(2)
              color: "transparent"
              border.width: 1
              // `stop` is the one that cannot be taken back, so it is the one
              // that looks different.
              border.color: pill.modelData === "stop" ? root.urgent : root.foreground
              opacity: pillMouse.containsMouse ? 0.75 : 0.30

              Text {
                id: pillLabel
                anchors.centerIn: parent
                text: String(pill.modelData)
                color: root.dim
                font.family: root.fontFamily
                font.pixelSize: Style.font.caption
              }

              MouseArea {
                id: pillMouse
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                // Set the cursor as well as acting: a pill that stole the hover
                // without moving the cursor would leave the highlight on
                // whatever row you came from.
                onEntered: root.setRepoCursor(wk.rowIndex)
                onClicked: wk.act(String(pill.modelData))
              }
            }
          }
        }
      }
    }

    // The progress rail, borrowed from the slot rail: a 3 px Item with a
    // background Rectangle at 0.10 and a filled one over it. Only drawn for a
    // batch that knows its own size — bound to 0/0 it would show a full bar for
    // work that has not started.
    Item {
      visible: wk.hasRail
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.top: wkBody.bottom
      anchors.topMargin: Style.space(3)
      anchors.leftMargin: Style.space(7)
      height: 3

      Rectangle {
        anchors.fill: parent
        color: root.foreground
        opacity: 0.10
      }
      Rectangle {
        anchors.left: parent.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        width: parent.width * Math.max(0, Math.min(1, wk.progress))
        color: root.stateColorFor("settled")
      }
    }
  }

  component AttentionRow: Item {
    id: att
    property var item: null
    property int rowIndex: 0

    readonly property bool isMore: att.item ? att.item.kind === "more" : false
    readonly property string groupLabel: att.item ? String(att.item.groupLabel || "") : ""
    readonly property string target: att.item ? String(att.item.target || "") : ""

    // A repo is in this band because something is wrong with it; what its
    // worktree actually holds is the part of that nobody has said yet.
    readonly property string detail: att.isMore
      ? String(att.item.members.join("  "))
      : root.repoDetailFor(att.target)

    readonly property string tipText: {
      if (!att.item) return ""
      if (att.isMore) return String(att.item.members.join(", "))
      var bits = []
      if (att.item.reason !== "") bits.push(String(att.item.reason))
      if (att.item.action !== "") bits.push(String(att.item.action))
      bits.push("\u23ce  the full report")
      return bits.join("\n")
    }

    function activate() {
      if (att.isMore) { root.toggleCause(att.item.groupKey); return }
      shipwright.openReport(att.target)
    }

    // A heading belongs to what is BELOW it. The gap that separates one group
    // from the next has to sit ABOVE the heading, not between the heading and
    // its own rows — put it between and you push the label away from the thing
    // it labels while leaving it touching the group above, which reads as if it
    // belonged to that one. Proximity is the only thing saying what is in which
    // section, so it has to point the right way: 10 above, 2 below.
    implicitHeight: attBody.implicitHeight
      + (groupLabel !== ""
         ? Style.space(10) + attHeader.implicitHeight + Style.space(2)
         : 0)

    // The command, stated once for the whole group.
    Row {
      id: attHeader
      visible: att.groupLabel !== ""
      anchors.top: parent.top
      anchors.topMargin: Style.space(10)
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.leftMargin: Style.space(7)
      anchors.rightMargin: 0
      spacing: Style.space(8)

      Text {
        id: attHeaderCmd
        text: att.groupLabel
        color: root.foreground
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
        font.bold: true
      }

      Item {
        width: Math.max(0, parent.width - attHeaderCmd.implicitWidth
                          - attHeaderCount.implicitWidth - parent.spacing * 2)
        height: 1
        anchors.verticalCenter: parent.verticalCenter
        Rectangle {
          anchors.verticalCenter: parent.verticalCenter
          width: parent.width
          height: 1
          color: root.urgent
          opacity: 0.25
        }
      }

      Text {
        id: attHeaderCount
        text: att.item ? att.item.groupCount : ""
        color: root.urgent
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }
    }

    CursorSurface {
      id: attBody
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.bottom: parent.bottom
      implicitHeight: attText.implicitHeight + Style.space(4)
      height: implicitHeight

      hasCursor: root.page === 0 && root.cursorActive
                 && root.repoIndex === att.rowIndex
      foreground: root.foreground

      MouseArea {
        id: attMouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onEntered: root.setRepoCursor(att.rowIndex)
        onClicked: att.activate()
      }

      // A ToolTip places itself ABOVE its parent by default. On a 20 px row in a
      // stacked list that lands squarely on the row above. Put it under the row
      // it explains, clamped into that row's own width.
      PanelToolTip {
        x: Math.min(Style.space(7), Math.max(0, attBody.width - width))
        y: attBody.height + Style.space(2)
        visible: attMouse.containsMouse && att.tipText !== ""
        text: att.tipText
        fontFamily: root.fontFamily
      }

      // The only place urgent appears above the fleet.
      Rectangle {
        anchors.left: parent.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        width: Style.space(2)
        color: att.isMore ? root.dim : root.urgent
      }

      Row {
        id: attRow
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        anchors.leftMargin: Style.space(7)
        anchors.rightMargin: 0
        spacing: Style.space(6)

        Text {
          id: attText
          text: att.item ? String(att.item.label) : ""
          color: att.isMore ? root.dim : root.foreground
          font.family: root.fontFamily
          font.pixelSize: Style.font.body
        }

        Text {
          id: attWhy
          width: Math.max(0, attRow.width - attText.implicitWidth
                          - attPill.width - attRow.spacing * 2)
          anchors.verticalCenter: parent.verticalCenter
          text: att.detail
          color: root.dim
          font.family: root.fontFamily
          font.pixelSize: Style.font.caption
          elide: Text.ElideRight
        }

        // A hairline pill, not a kit Button: a Button is 28 px and will not fit
        // a 20 px row.
        Rectangle {
          id: attPill
          anchors.verticalCenter: parent.verticalCenter
          width: openLabel.implicitWidth + Style.space(8)
          height: openLabel.implicitHeight + Style.space(2)
          color: "transparent"
          border.width: 1
          border.color: att.isMore ? root.foreground : root.urgent
          opacity: attMouse.containsMouse ? 0.75 : 0.30
          Text {
            id: openLabel
            anchors.centerIn: parent
            text: att.isMore
              ? (att.item.expanded ? "\u2212" : "+" + att.item.more)
              : "why"
            color: root.dim
            font.family: root.fontFamily
            font.pixelSize: Style.font.caption
          }
        }
      }
    }
  }

}
