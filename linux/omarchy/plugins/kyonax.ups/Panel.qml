import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Ui

import "Model.js" as Model

// The desk UPS in the Omarchy bar.
//
// SINGLE FILE ON PURPOSE (the kyonax.tempo-hours lesson): a third-party
// plugin's own directory is an implicit QML import, so a BarWidget.qml here
// would shadow the BarWidget type it extends and Qt would refuse the entry
// point with "File name case mismatch". Service.qml and Model.js are safe.
//
// Bar: the glyph says mains or battery and stays dim while all is well. On
// battery it carries the countdown to the clean shutdown, in the urgent colour.
// Left = popup · right = read now · middle = the outage log in a terminal.
Panel {
  id: root
  moduleName: "kyonax.ups"
  ipcTarget: "kyonax.ups"
  manageIpc: false

  // "test" | "shutdown" while a confirmation is up; "" otherwise.
  property string confirming: ""

  readonly property color foreground: bar ? bar.foreground : Color.foreground
  readonly property color urgent: bar ? bar.urgent : Color.urgent
  readonly property color dim: Qt.darker(foreground, 1.55)
  readonly property string fontFamily: bar ? bar.fontFamily : Style.font.family

  readonly property int refreshIntervalSec:
    (settings && settings.refreshIntervalSec !== undefined) ? settings.refreshIntervalSec : 5
  readonly property string barLabelMode:
    (settings && settings.barLabel !== undefined) ? settings.barLabel : "Icon only"

  // Where this file lives, for the helpers in bin/ and the setup hint.
  readonly property string pluginDir:
    decodeURIComponent(Qt.resolvedUrl(".").toString().replace(/^file:\/\//, "")).replace(/\/$/, "")

  readonly property var ups: service.reading
  readonly property var prot: service.protection
  readonly property var cd: service.countdown
  readonly property string uiState: service.uiState
  readonly property string glyph: Model.barGlyph(ups, prot)
  readonly property string label: Model.barLabel(ups, prot, cd, barLabelMode, service.shuttingDown)
  readonly property bool canControl: Model.canControl(ups, service.services)

  function stateColorFor(s) {
    if (s === "alert") return root.urgent
    if (s === "pending") return root.foreground
    return root.dim
  }
  readonly property color stateColor: stateColorFor(uiState)

  // Settled is dimmer than pending: a healthy UPS should not pull the eye.
  readonly property color barIconForeground:
    uiState === "pending" ? barForeground : Qt.darker(barForeground, 1.45)

  readonly property string barText:
    (bar && bar.vertical) || label === "" ? glyph : glyph + " " + label

  function ask(what) {
    root.confirming = what
    confirmDialog.selectedIndex = 0      // Cancel first: Enter alone never shuts anything down
    if (!root.opened) root.open()
  }

  function confirmNow() {
    var what = root.confirming
    root.confirming = ""
    if (what === "test") service.startTest()
    else if (what === "shutdown") service.shutdownNow()
  }

  function flipChoice() { confirmDialog.selectedIndex = confirmDialog.selectedIndex === 0 ? 1 : 0 }

  // WITHOUT THESE TWO LINES THE WIDGET IS INVISIBLE (kyonax.tempo-hours paid
  // for this one): the bar sizes a slot from the item's implicit size, and the
  // base Panel declares none — no error, no warning, no widget.
  implicitWidth: button.implicitWidth
  implicitHeight: button.implicitHeight

  Service {
    id: service
    refreshIntervalSec: root.refreshIntervalSec
    helper: root.pluginDir + "/bin/ups-instcmd"
    probe: root.pluginDir + "/bin/ups-probe"
  }

  // Reachable as  quickshell ipc -p /usr/share/omarchy/shell call kyonax.ups <fn>
  // — NOT via `omarchy-shell call`, which only reaches first-party targets.
  IpcHandler {
    target: root.ipcTarget
    function refresh(): string { service.refresh(); return "ok" }
    function state(): string { return root.uiState }
    function status(): string { return root.ups.ok ? root.ups.status : "error: " + root.ups.error }
    function diag(): string {
      return "opened=" + root.opened
        + " ok=" + root.ups.ok + " status=" + root.ups.status
        + " protected=" + root.prot.ok + "/" + root.prot.known
        + " timer=" + service.policy.timerSec
        + " events=" + service.events.length
        + " countdown=" + (root.cd ? Math.round(root.cd.remainingMs / 1000) + "s" : "none")
        + " control=" + root.canControl
        + " confirming=" + root.confirming
        + " dir=" + root.pluginDir
    }
    // The same code paths the buttons take, so a silent button can be told
    // apart from an unwired one. The battery test and the shutdown only RAISE
    // their confirmation over IPC; a person still has to accept it.
    function beeper(): string { service.toggleBeeper(); return "ok" }
    function log(): string { service.openLog(); return "ok" }
    function askTest(): string { root.ask("test"); return "ok" }
    function askShutdown(): string { root.ask("shutdown"); return "ok" }

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
    text: root.barText
    active: root.uiState === "alert"
    foreground: root.barIconForeground
    // Estimated, not measured (the label lives here, the painted item does
    // not); the trailing constant keeps the text off the next widget.
    slotSize: (root.bar && root.bar.vertical) || root.label === ""
      ? Style.bar.statusSlot
      : Style.bar.statusSlot + Math.round(root.label.length * Style.font.bodySmall * 0.72) + Style.space(10)
    tooltipText: Model.tooltip(root.ups, root.prot, root.cd)

    onPressed: function (buttonCode) {
      if (buttonCode === Qt.RightButton) service.refresh()
      else if (buttonCode === Qt.MiddleButton) service.openLog()
      else root.toggle()
    }
  }

  onOpenedChanged: {
    if (opened) {
      if (panelFlick) panelFlick.contentY = 0
      service.refresh()
    } else {
      root.confirming = ""
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
    contentHeight: panel.fittedContentHeight(column.implicitHeight, Style.space(640))

    PanelKeyCatcher {
      id: keyCatcher
      anchors.fill: parent

      // While a confirmation is up the keys belong to it: arrows or Tab switch
      // the choice, Enter/Space take it, Esc or n backs out, y confirms.
      onCloseRequested: {
        if (root.confirming !== "") root.confirming = ""
        else root.close()
      }
      onMoveRequested: function (dx, dy) {
        if (root.confirming !== "" && dx !== 0) root.flipChoice()
      }
      onTabRequested: function (direction) {
        if (root.confirming !== "") root.flipChoice()
      }
      onActivateRequested: {
        if (root.confirming === "") return
        if (confirmDialog.selectedIndex === 1) root.confirmNow()
        else root.confirming = ""
      }
      onTextKey: function (t) {
        var k = t.toLowerCase()
        if (root.confirming !== "") {
          if (k === "y") root.confirmNow()
          else if (k === "n") root.confirming = ""
          return
        }
        if (k === "r") service.refresh()
        else if (k === "b") { if (root.canControl) service.toggleBeeper() }
        else if (k === "t") { if (root.canControl && !root.ups.onBattery) root.ask("test") }
        else if (k === "e") service.openLog()
        else if (k === "s") root.ask("shutdown")
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
            title: service.loaded ? Model.title(root.ups, root.cd, service.shuttingDown) : "Reading the UPS…"
            meta: service.loaded
              ? Model.heroMeta(root.ups, root.prot, service.policy, root.cd, service.shuttingDown) : ""
            detail: Model.heroPill(root.prot)
            foreground: root.foreground
            fontFamily: root.fontFamily
            iconComponent: Component {
              Text {
                text: root.glyph
                color: root.stateColor
                font.family: root.fontFamily
                font.pixelSize: Style.font.display
              }
            }
            trailingControl: Component {
              PanelActionButton {
                iconText: Model.G.refresh
                foreground: root.foreground
                fontFamily: root.fontFamily
                tooltipText: "Read the UPS now  (r)"
                onClicked: service.refresh()
              }
            }
          }

          // The countdown, drawn: a rail that fills toward the clean shutdown.
          Column {
            visible: root.cd !== null
            width: parent.width
            spacing: Style.space(6)

            Rectangle {
              width: parent.width
              height: Style.space(4)
              radius: height / 2
              color: Util.alpha(root.foreground, 0.14)

              Rectangle {
                width: root.cd ? parent.width * Math.min(1, root.cd.elapsedMs / root.cd.totalMs) : 0
                height: parent.height
                radius: parent.radius
                color: root.urgent
              }
            }
            Text {
              width: parent.width
              text: root.cd ? "Clean shutdown at " + Model.clockTime(root.cd.deadlineMs)
                              + " unless power returns. Save your work now." : ""
              color: root.urgent
              font.family: root.fontFamily
              font.pixelSize: Style.font.bodySmall
              wrapMode: Text.WordWrap
            }
          }

          Text {
            visible: service.loaded && (!root.ups.ok || (root.prot.known && !root.prot.ok))
            width: parent.width
            text: !root.ups.ok
              ? "No readings: " + root.ups.error + ". Check: systemctl status nut-driver@ups nut-server"
              : "The PC is NOT protected: " + root.prot.reason + ". A long outage would cut it off hard."
            color: root.urgent
            font.family: root.fontFamily
            font.pixelSize: Style.font.bodySmall
            wrapMode: Text.WordWrap
          }

          RowLayout {
            visible: root.ups.ok
            width: parent.width
            spacing: 0

            Repeater {
              model: Model.tiles(root.ups)
              delegate: StatTile {
                required property var modelData
                required property int index
                Layout.fillWidth: true
                Layout.preferredWidth: 1
                value: modelData.value
                label: modelData.label
                alert: modelData.alert
                first: index === 0
              }
            }
          }

          PanelSeparator { visible: root.ups.ok; foreground: root.foreground }

          Column {
            visible: root.ups.ok
            width: parent.width
            spacing: Style.spacing.labelGap

            PanelSectionHeader { text: "READINGS"; foreground: root.foreground; fontFamily: root.fontFamily }
            Repeater {
              model: Model.detailRows(root.ups)
              delegate: InfoPair {
                required property var modelData
                label: modelData.label
                value: modelData.value
              }
            }
          }

          PanelSeparator { foreground: root.foreground }

          Column {
            width: parent.width
            spacing: Style.spacing.labelGap

            PanelSectionHeader { text: "PROTECTION"; foreground: root.foreground; fontFamily: root.fontFamily }
            InfoPair { label: "Policy"; value: Model.policyText(service.policy) }
            Repeater {
              model: Model.serviceRows(service.services)
              delegate: InfoPair {
                required property var modelData
                label: modelData.label
                value: modelData.value
                alert: modelData.alert
              }
            }
          }

          PanelSeparator { foreground: root.foreground }

          Column {
            width: parent.width
            spacing: Style.spacing.labelGap

            PanelSectionHeader { text: "OUTAGES"; foreground: root.foreground; fontFamily: root.fontFamily }
            InfoPair { label: "Last"; value: Model.lastOutageText(service.outageList, service.nowMs) }
            Repeater {
              model: Model.eventRows(service.events, service.nowMs, 6)
              delegate: EventRow {
                required property var modelData
                entry: modelData
              }
            }
          }

          PanelSeparator { foreground: root.foreground }

          Row {
            spacing: Style.space(8)

            PanelActionButton {
              iconText: Model.beeperOn(root.ups) ? Model.G.bell : Model.G.bellOff
              foreground: root.foreground
              fontFamily: root.fontFamily
              enabled: root.canControl && !service.controlling
              tooltipText: Model.beeperOn(root.ups) ? "Mute the UPS beeper  (b)" : "Turn the UPS beeper on  (b)"
              onClicked: service.toggleBeeper()
            }
            PanelActionButton {
              iconText: Model.G.test
              foreground: root.foreground
              fontFamily: root.fontFamily
              enabled: root.canControl && !service.controlling && !root.ups.onBattery
              tooltipText: "Quick battery test, about 10 s  (t)"
              onClicked: root.ask("test")
            }
            PanelActionButton {
              iconText: Model.G.log
              foreground: root.foreground
              fontFamily: root.fontFamily
              tooltipText: "Open the outage log  (e)"
              onClicked: service.openLog()
            }
            PanelActionButton {
              iconText: Model.G.power
              foreground: root.foreground
              hoverColor: root.urgent
              fontFamily: root.fontFamily
              tooltipText: "Shut the PC down now  (s)"
              onClicked: root.ask("shutdown")
            }
          }

          Text {
            visible: service.feedback !== ""
            width: parent.width
            text: service.feedback
            color: service.feedbackError ? root.urgent : root.foreground
            font.family: root.fontFamily
            font.pixelSize: Style.font.bodySmall
            wrapMode: Text.WordWrap
          }

          Text {
            visible: text !== ""
            width: parent.width
            text: Model.controlHint(root.ups, service.services, root.pluginDir)
            color: root.foreground
            opacity: 0.55
            font.family: root.fontFamily
            font.pixelSize: Style.font.caption
            wrapMode: Text.WrapAnywhere
          }

          Text {
            width: parent.width
            text: "r read · b beeper · t test · e log · s shut down"
            color: root.foreground
            opacity: 0.45
            font.family: root.fontFamily
            font.pixelSize: Style.font.caption
            wrapMode: Text.WordWrap
          }
        }
      }

      ConfirmDialog {
        id: confirmDialog
        anchors.fill: parent
        z: 10
        opened: root.confirming !== ""
        message: root.confirming === "shutdown"
          ? "Shut the PC down now? Apps are closed first, like Omarchy's own shutdown."
          : "Run a 10-second battery test? The PC runs on the UPS battery meanwhile. A worn battery could drop the PC, so save your work first."
        confirmText: root.confirming === "shutdown" ? "Shut down" : "Start test"
        foreground: root.foreground
        fontFamily: root.fontFamily
        onCanceled: root.confirming = ""
        onConfirmed: root.confirmNow()
      }
    }
  }

  // ---- components ---------------------------------------------------------

  // The shipwright tile: a number over a small caption, hairline between.
  component StatTile: Item {
    id: tile
    property string value: ""
    property string label: ""
    property bool alert: false
    property bool first: false
    implicitHeight: tileText.implicitHeight

    Rectangle {
      visible: !tile.first
      width: 1
      height: parent.height
      color: root.foreground
      opacity: 0.12
    }
    Column {
      id: tileText
      x: tile.first ? 0 : Style.space(8)
      spacing: 0
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
  }

  component InfoPair: RowLayout {
    id: pair
    property string label: ""
    property string value: ""
    property bool alert: false
    width: parent ? parent.width : 0
    spacing: Style.space(8)

    Text {
      text: pair.label
      color: root.foreground
      opacity: 0.6
      font.family: root.fontFamily
      font.pixelSize: Style.font.bodySmall
    }
    Text {
      Layout.fillWidth: true
      horizontalAlignment: Text.AlignRight
      text: pair.value
      color: pair.alert ? root.urgent : root.foreground
      font.family: root.fontFamily
      font.pixelSize: Style.font.bodySmall
      elide: Text.ElideRight
    }
  }

  component EventRow: RowLayout {
    id: eventRow
    property var entry: null
    width: parent ? parent.width : 0
    spacing: Style.space(8)

    Rectangle {
      Layout.alignment: Qt.AlignVCenter
      implicitWidth: Style.space(6)
      implicitHeight: Style.space(6)
      radius: width / 2
      color: eventRow.entry && eventRow.entry.alert ? root.urgent : root.dim
    }
    Text {
      Layout.fillWidth: true
      text: eventRow.entry ? eventRow.entry.text : ""
      color: root.foreground
      font.family: root.fontFamily
      font.pixelSize: Style.font.bodySmall
      elide: Text.ElideRight
    }
    Text {
      text: eventRow.entry ? eventRow.entry.when : ""
      color: root.dim
      font.family: root.fontFamily
      font.pixelSize: Style.font.caption
    }
  }
}
