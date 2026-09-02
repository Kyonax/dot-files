import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Ui

import "Model.js" as Model

// Hours owed to Tempo, in the Omarchy bar.
//
// SINGLE FILE ON PURPOSE. First-party widgets split the bar element into
// BarWidget.qml and the popup into Panel.qml, but that split only works from
// the packaged type root: a third-party plugin's own directory is an implicit
// QML import, so a file named after a type it inherits from shadows that type,
// and Qt refuses the entry point with "File name case mismatch" — an error a
// long way from what is actually wrong.
//
// Left click opens the breakdown, right click re-checks against Tempo, middle
// click opens Tempo — because the one act this tool refuses to automate is
// clicking Submit, and Tempo is where you go to do it.
Panel {
  id: root
  moduleName: "kyonax.tempo-hours"
  ipcTarget: "kyonax.tempo-hours"
  manageIpc: false

  property int dayIndex: 0
  property bool cursorActive: false

  readonly property color foreground: bar ? bar.foreground : Color.foreground
  readonly property color urgent: bar ? bar.urgent : Color.urgent
  readonly property color dim: Qt.darker(foreground, 1.55)
  readonly property string fontFamily: bar ? bar.fontFamily : Style.font.family

  readonly property int refreshIntervalSec:
    (settings && settings.refreshIntervalSec !== undefined) ? settings.refreshIntervalSec : 900

  readonly property var snap: service.snap
  readonly property string label: Model.barText(snap)
  readonly property string uiState: Model.state(snap)

  function stateColorFor(s) {
    if (s === "alert") return root.urgent
    if (s === "pending") return root.foreground
    return root.dim
  }
  readonly property color stateColor: stateColorFor(uiState)

  readonly property bool barAlert:
    !service.available || service.lastError !== "" || uiState === "alert"

  // Settled is deliberately dimmer than pending: a tool with nothing to say
  // should not pull the eye. Colour comes from the theme, never a literal.
  readonly property color barIconForeground:
    uiState === "pending" ? barForeground : Qt.darker(barForeground, 1.45)

  // The bar carries the glyph plus the number, which is the whole question the
  // owner wants answered without opening anything: do I owe Tempo hours?
  readonly property string barLabel:
    (bar && bar.vertical) ? "󰥔" : (label === "0h" ? "󰥔" : "󰥔 " + label)

  function moveCursor(dx, dy) {
    var n = snap.days.length
    if (n === 0) return
    root.dayIndex = Math.max(0, Math.min(n - 1, root.dayIndex + dy + dx))
  }

  // WITHOUT THESE TWO LINES THE WIDGET IS INVISIBLE.
  //
  // The bar sizes a slot from its item's implicit size, and the base Panel
  // declares none. `BarIconButton { anchors.fill: parent }` then fills a
  // zero-width parent and paints nothing — no error, no warning, no widget.
  // It cost an afternoon; do not remove them.
  implicitWidth: button.implicitWidth
  implicitHeight: button.implicitHeight

  Service {
    id: service
    refreshIntervalSec: root.refreshIntervalSec
  }

  // A finished run pushes a refresh in rather than waiting out the interval to
  // be noticed. Reachable as:
  //   quickshell ipc -p /usr/share/omarchy/shell call kyonax.tempo-hours refresh
  // NOT via `omarchy-shell call`, which only reaches first-party targets.
  IpcHandler {
    target: root.ipcTarget
    function refresh(): string { service.refresh(); return "ok" }
    function diag(): string {
      return "opened=" + root.opened
        + " quickRunning=" + service.quickProcess.running
        + " fullRunning=" + service.busy
        + " appliedSeq=" + service.appliedSeq
        + " seq=" + service.seq
    }
    function state(): string { return root.uiState }
    function owed(): string { return root.label }
    // Exposed so the same code path a button click takes can be exercised
    // without a click. This is how the buttons were debugged, and it is worth
    // keeping: a button that silently does nothing is indistinguishable from
    // one that is not wired up, and `quickshell ipc ... call ... review` tells
    // the two apart in a second.
    function review(): string { service.review(); return "ok" }
    function capture(): string { service.capture(); return "ok" }
    function dryRun(): string { service.runNow(true); return "ok" }
    function tempo(): string { service.openTempo(); return "ok" }

    function open(): void { root.open() }
    function close(): void { root.close() }
    function show(): void { root.open() }
    function hide(): void { root.close() }
    function toggle(): void { root.toggle() }
  }

  BarIconButton {
    id: button
    anchors.fill: parent
    bar: root.bar
    text: root.barLabel
    active: root.barAlert
    foreground: root.barIconForeground
    // The glyph alone fits the status slot; the glyph plus "48h owed" does not.
    // Estimated rather than measured because the label lives in this scope but
    // the painted item does not.
    // Estimated, not measured: the label lives in this scope but the painted
    // item does not. The trailing constant is breathing room — without it the
    // text butts straight into whatever widget sits next along the bar.
    slotSize: (root.bar && root.bar.vertical) || root.label === "0h"
      ? Style.bar.statusSlot
      : Style.bar.statusSlot + Math.round(root.label.length * Style.font.bodySmall * 0.72) + Style.space(10)
    tooltipText: Model.runText(root.snap)

    onPressed: function (buttonCode) {
      if (buttonCode === Qt.RightButton) service.refresh()
      else if (buttonCode === Qt.MiddleButton) service.openTempo()
      else root.toggle()
    }
  }

  // Show the cached payload instantly, then correct it from Tempo.
  //
  // The cheap read replays the last full fetch, so it opens with exactly what
  // the bar is showing — the two can no longer disagree. The full refresh that
  // follows is what makes an open self-correcting rather than a snapshot of
  // whenever the timer last ran.
  onOpenedChanged: if (opened) {
    cursorActive = false
    if (panelFlick) panelFlick.contentY = 0
    service.refreshQuick()
    Qt.callLater(function () { service.refresh() })
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
      onMoveRequested: function (dx, dy) {
        if (!root.cursorActive) { root.cursorActive = true; return }
        root.moveCursor(dx, dy)
      }
      onCloseRequested: root.close()
      onTextKey: function (t) {
        if (t === "r" || t === "R") service.refresh()
        else if (t === "d" || t === "D") service.review()
        else if (t === "n" || t === "N") service.runNow(true)
        else if (t === "c" || t === "C") service.capture()
        else if (t === "t" || t === "T") service.openTempo()
      }

      Flickable {
        id: panelFlick
        anchors.fill: parent
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
          spacing: Style.space(12)

          PanelHero {
            width: parent.width
            title: root.label === "0h" ? "Nothing owed" : root.label
            meta: service.loaded ? Model.runText(root.snap) : "Asking Tempo…"
            detail: Model.armedLabel(root.snap)
                    + (Model.ageText(root.snap) ? "  ·  " + Model.ageText(root.snap) : "")
            foreground: root.foreground
            fontFamily: root.fontFamily
            iconComponent: Component {
              Text {
                text: "󰥔"
                color: root.stateColor
                font.family: root.fontFamily
                font.pixelSize: Style.font.display
              }
            }
            trailingControl: Component {
              PanelActionButton {
                iconText: "󰑐"
                foreground: root.foreground
                fontFamily: root.fontFamily
                // Never disabled. A press while a check is in flight is
                // remembered and honoured when it lands.
                onClicked: service.refresh()
                PanelToolTip {
                  visible: parent.containsMouse === true
                  text: service.busy ? "Checking…" : "Re-check against Tempo"
                  fontFamily: root.fontFamily
                }
              }
            }
          }

          Text {
            visible: service.lastError !== ""
            width: parent.width
            text: service.lastError
            color: root.urgent
            font.family: root.fontFamily
            font.pixelSize: Style.font.bodySmall
            wrapMode: Text.WordWrap
          }

          Column {
            visible: service.loaded && service.lastError === ""
            width: parent.width
            spacing: Style.spacing.labelGap

            InfoPair { label: "Owed";   value: Model.hours(root.snap.gapMinutes) }
            InfoPair { label: "Period"; value: Model.periodText(root.snap) }
            InfoPair { label: "Captured"; value: Model.capturesText(root.snap) }
            InfoPair {
              label: "Scheduler"
              value: root.snap.paused ? "paused"
                   : (root.snap.armed ? "armed — Friday will push"
                                      : "disarmed — drafts only")
            }
          }

          PanelSeparator {
            visible: root.snap.days.length > 0
            foreground: root.foreground
          }

          Column {
            visible: root.snap.days.length > 0
            width: parent.width
            spacing: Style.space(6)

            PanelSectionHeader {
              text: "UNLOGGED DAYS"
              foreground: root.foreground
              fontFamily: root.fontFamily
            }

            Repeater {
              model: root.snap.days
              delegate: DayRow {
                required property var modelData
                required property int index
                width: parent.width
                day: modelData
                rowIndex: index
              }
            }
          }

          PanelSeparator {
            visible: root.snap.themes.length > 0
            foreground: root.foreground
          }

          Column {
            visible: root.snap.themes.length > 0
            width: parent.width
            spacing: Style.space(4)

            PanelSectionHeader {
              text: "DRAFT"
              foreground: root.foreground
              fontFamily: root.fontFamily
            }

            Repeater {
              model: root.snap.themes.slice(0, 8)
              delegate: Row {
                required property var modelData
                width: parent.width
                spacing: Style.space(8)

                Text {
                  text: Model.hours(modelData.minutes)
                  color: root.foreground
                  opacity: 0.6
                  font.family: root.fontFamily
                  font.pixelSize: Style.font.caption
                  width: Style.space(34)
                }
                Text {
                  text: modelData.text
                  color: root.foreground
                  font.family: root.fontFamily
                  font.pixelSize: Style.font.caption
                  elide: Text.ElideRight
                  width: parent.width - Style.space(42)
                }
              }
            }
          }

          PanelSeparator { foreground: root.foreground }

          Row {
            spacing: Style.space(8)

            PanelActionButton {
              iconText: "󰆓"; foreground: root.foreground; fontFamily: root.fontFamily
              onClicked: service.capture()
              PanelToolTip { visible: parent.containsMouse === true
                             text: "Capture missing days now  (c)"; fontFamily: root.fontFamily }
            }
            PanelActionButton {
              iconText: "󰈙"; foreground: root.foreground; fontFamily: root.fontFamily
              onClicked: service.review()
              PanelToolTip { visible: parent.containsMouse === true
                             text: "Read the draft  (d)"; fontFamily: root.fontFamily }
            }
            PanelActionButton {
              iconText: "󰐊"; foreground: root.foreground; fontFamily: root.fontFamily
              onClicked: service.runNow(true)
              PanelToolTip { visible: parent.containsMouse === true
                             text: "Go — fill the hours now  (n)"; fontFamily: root.fontFamily }
            }
            PanelActionButton {
              iconText: "󰖟"; foreground: root.foreground; fontFamily: root.fontFamily
              onClicked: service.openTempo()
              PanelToolTip { visible: parent.containsMouse === true
                             text: "Open Tempo to Submit  (t)"; fontFamily: root.fontFamily }
            }
          }

          Text {
            width: parent.width
            text: "Worklogs are never submitted for you. That stays your click."
            color: root.foreground
            opacity: 0.45
            font.family: root.fontFamily
            font.pixelSize: Style.font.caption
            wrapMode: Text.WordWrap
          }
        }
      }
    }
  }

  // ---- row and label components -----------------------------------------

  component DayRow: CursorSurface {
    id: dayRow
    property var day: null
    property int rowIndex: 0

    readonly property string state: Model.dayState(day)
    readonly property color accentColor: root.stateColorFor(state)

    hasCursor: root.cursorActive && root.dayIndex === rowIndex
    foreground: root.foreground
    implicitHeight: dayContent.implicitHeight + Style.spacing.rowPaddingX

    RowLayout {
      id: dayContent
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.verticalCenter: parent.verticalCenter
      spacing: Style.space(8)

      Text {
        text: dayRow.day ? dayRow.day.date : ""
        color: root.foreground
        font.family: root.fontFamily
        font.pixelSize: Style.font.bodySmall
      }
      Text {
        text: dayRow.day && dayRow.day.label ? dayRow.day.label : ""
        color: root.foreground
        opacity: 0.55
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }
      Item { Layout.fillWidth: true }
      Text {
        visible: dayRow.day && dayRow.day.thin
        text: "no evidence"
        color: dayRow.accentColor
        opacity: 0.8
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
      }
      Text {
        text: dayRow.day ? Model.hours(dayRow.day.minutes) : ""
        color: root.foreground
        font.family: root.fontFamily
        font.pixelSize: Style.font.bodySmall
      }
    }
  }

  component InfoPair: Row {
    property string label: ""
    property string value: ""
    width: parent.width
    spacing: Style.space(8)

    InfoLabel { text: label }
    Item {
      width: Math.max(0, parent.width - parent.children[0].implicitWidth
                         - parent.children[2].implicitWidth - parent.spacing * 2)
      height: 1
    }
    InfoValue { text: value }
  }

  component InfoLabel: Text {
    color: root.foreground
    opacity: 0.6
    font.family: root.fontFamily
    font.pixelSize: Style.font.bodySmall
  }

  component InfoValue: Text {
    color: root.foreground
    font.family: root.fontFamily
    font.pixelSize: Style.font.bodySmall
    elide: Text.ElideRight
  }
}
