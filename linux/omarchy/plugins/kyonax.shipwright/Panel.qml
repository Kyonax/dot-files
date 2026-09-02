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

  readonly property color foreground: bar ? bar.foreground : Color.foreground
  readonly property color urgent: bar ? bar.urgent : Color.urgent
  readonly property color dim: Qt.darker(foreground, 1.55)
  readonly property string fontFamily: bar ? bar.fontFamily : Style.font.family

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
  readonly property color barIconForeground: shipwright.fleetState === "pending" ? barForeground : Qt.darker(barForeground, 1.45)

  function stateColorFor(state) {
    if (state === "alert") return urgent
    if (state === "pending") return foreground
    return dim
  }

  function ensureCursor() {
    if (shipwright.repos.length === 0) { repoIndex = 0; return }
    if (repoIndex >= shipwright.repos.length) repoIndex = shipwright.repos.length - 1
    if (repoIndex < 0) repoIndex = 0
  }

  function moveCursor(dx, dy) {
    cursorActive = true
    ensureCursor()
    if (dy === 0 || shipwright.repos.length === 0) return
    repoIndex = Math.max(0, Math.min(shipwright.repos.length - 1, repoIndex + dy))
    scrollCursorIntoView()
  }

  function setRepoCursor(index) {
    cursorActive = true
    repoIndex = index
    scrollCursorIntoView()
  }

  function selectedRepo() {
    if (shipwright.repos.length === 0) return null
    return shipwright.repos[Math.max(0, Math.min(repoIndex, shipwright.repos.length - 1))]
  }

  // Enter on a repo opens its ledger -- the run-by-run record of what
  // shipwright actually did to it, which is the question a row provokes.
  function activateCursor() {
    var repo = selectedRepo()
    if (!repo) return
    shipwright.launchTui("shipwright-ledger", "shipwright ledger --repo " + repo.name)
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

  function scrollCursorIntoView() {
    if (repoColumn && repoIndex >= 0 && repoIndex < repoColumn.children.length) {
      scrollItemIntoView(repoColumn.children[repoIndex])
    }
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
    Qt.callLater(function() { keyCatcher.forceActiveFocus() })
  }
  onRepoIndexChanged: scrollCursorIntoView()

  Service {
    id: shipwright
    settings: root.settings
  }

  Connections {
    target: shipwright
    function onRefreshed() { root.ensureCursor() }
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
        + " quickRunning=" + shipwright.quickRunning
        + " fullRunning=" + shipwright.busy
        + " appliedSeq=" + shipwright.appliedSeq
        + " seq=" + shipwright.seq
    }
  }

  BarIconButton {
    id: button
    anchors.fill: parent
    bar: root.bar
    text: "󰻈"
    active: root.barAlert
    foreground: root.barIconForeground
    slotSize: Style.bar.statusSlot
    tooltipText: ""

    onPressed: function(buttonCode) {
      if (buttonCode === Qt.RightButton) shipwright.refresh()
      else if (buttonCode === Qt.MiddleButton) shipwright.launchTui("shipwright-logs", "shipwright logs --list")
      else root.toggle()
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
      onTextKey: function(t) {
        if (t === "r" || t === "R") shipwright.refresh()
        else if (t === "l" || t === "L") shipwright.launchTui("shipwright-logs", "shipwright logs --list")
        else if (t === "h" || t === "H") shipwright.launchTui("shipwright-health-dash", "shipwright health")
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
            id: hero
            width: parent.width
            title: "Shipwright"
            meta: shipwright.loaded ? shipwright.runText : "Checking fleet…"
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
              PanelActionButton {
                iconText: "󰑐"
                foreground: root.foreground
                fontFamily: root.fontFamily
                // Never disabled. A press while a check is in flight is now
                // remembered and honoured when it lands, so the button that
                // says "re-check" always does.
                onClicked: shipwright.refresh()

                PanelToolTip {
                  visible: parent.containsMouse === true
                  text: shipwright.busy ? "Checking… (press to re-check again)" : "Re-check every repo"
                  fontFamily: root.fontFamily
                }
              }
            }
          }

          Text {
            visible: shipwright.lastError !== ""
            width: parent.width
            text: shipwright.lastError
            color: root.urgent
            font.family: root.fontFamily
            font.pixelSize: Style.font.bodySmall
            wrapMode: Text.WordWrap
          }

          Column {
            visible: shipwright.loaded && shipwright.lastError === ""
            width: parent.width
            spacing: Style.spacing.labelGap

            InfoPair { label: "Last run"; value: shipwright.runSlotText }
            InfoPair { label: "Fleet"; value: shipwright.fleetSummaryText }
          }

          PanelSeparator {
            visible: shipwright.repos.length > 0
            foreground: root.foreground
          }

          Column {
            visible: shipwright.repos.length > 0
            width: parent.width
            spacing: Style.space(10)

            PanelSectionHeader {
              text: "REPOSITORIES"
              foreground: root.foreground
              fontFamily: root.fontFamily
            }

            Column {
              id: repoColumn
              width: parent.width
              spacing: Style.space(6)

              Repeater {
                model: shipwright.repos
                RepoRow {
                  required property var modelData
                  required property int index
                  width: repoColumn.width
                  repo: modelData
                  rowIndex: index
                }
              }
            }
          }

          PanelSeparator {
            visible: shipwright.prs.length > 0
            foreground: root.foreground
          }

          // PULL REQUESTS
          //
          // The browser rule in 90-report.sh refuses to open a PR raised in a
          // night slot, on purpose. This section is the morning surface for it:
          // whatever the fleet raised while nobody was looking is listed here,
          // newest last, and a row is a link.
          Column {
            visible: shipwright.prs.length > 0
            width: parent.width
            spacing: Style.space(10)

            Row {
              width: parent.width
              spacing: Style.space(8)

              PanelSectionHeader {
                text: "PULL REQUESTS"
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

            Column {
              id: prColumn
              width: parent.width
              spacing: Style.space(6)

              Repeater {
                model: shipwright.prs
                PrRow {
                  required property var modelData
                  width: prColumn.width
                  pr: modelData
                }
              }
            }
          }

          PanelSeparator {
            visible: shipwright.notes.length > 0
            foreground: root.foreground
          }

          Column {
            visible: shipwright.notes.length > 0
            width: parent.width
            spacing: Style.space(6)

            PanelSectionHeader {
              text: "NOTES"
              foreground: root.foreground
              fontFamily: root.fontFamily
            }

            Repeater {
              model: shipwright.notes
              Text {
                required property var modelData
                width: parent.width
                text: "· " + modelData
                color: root.dim
                font.family: root.fontFamily
                font.pixelSize: Style.font.caption
                wrapMode: Text.WordWrap
              }
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
      onClicked: shipwright.openUrl(prRow.pr ? prRow.pr.url : "")
    }

    PanelToolTip {
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

  component RepoRow: CursorSurface {
    id: repoRow
    property var repo: null
    property int rowIndex: 0

    readonly property string state: Model.repoState(repo)
    readonly property color accentColor: root.stateColorFor(state)

    hasCursor: root.cursorActive && root.repoIndex === rowIndex
    foreground: root.foreground

    implicitHeight: repoContent.implicitHeight + Style.spacing.rowPaddingX

    MouseArea {
      id: repoMouse
      anchors.fill: parent
      hoverEnabled: true
      cursorShape: Qt.PointingHandCursor
      onEntered: root.setRepoCursor(repoRow.rowIndex)
      onClicked: root.activateCursor()
    }

    PanelToolTip {
      visible: repoMouse.containsMouse && Model.repoTooltip(repoRow.repo) !== ""
      text: Model.repoTooltip(repoRow.repo)
      fontFamily: root.fontFamily
    }

    RowLayout {
      anchors.left: parent.left
      anchors.right: parent.right
      anchors.verticalCenter: parent.verticalCenter
      anchors.leftMargin: Style.space(10)
      anchors.rightMargin: Style.space(10)
      spacing: Style.space(8)

      // A dot rather than a per-state glyph: the colour already carries the
      // meaning, and six identical shapes keep the list scannable.
      Rectangle {
        width: Style.space(7)
        height: Style.space(7)
        radius: width / 2
        color: repoRow.accentColor
        opacity: repoRow.state === "settled" ? 0.55 : 1.0
        Layout.alignment: Qt.AlignVCenter
      }

      ColumnLayout {
        id: repoContent
        Layout.fillWidth: true
        spacing: Style.space(1)

        Text {
          Layout.fillWidth: true
          text: repoRow.repo ? repoRow.repo.name : ""
          color: root.foreground
          font.family: root.fontFamily
          font.pixelSize: Style.font.body
          elide: Text.ElideRight
        }

        Text {
          Layout.fillWidth: true
          text: Model.repoDetailText(repoRow.repo)
          color: root.dim
          font.family: root.fontFamily
          font.pixelSize: Style.font.caption
          elide: Text.ElideRight
        }
      }

      Text {
        text: Model.repoOutcomeText(repoRow.repo)
        color: repoRow.accentColor
        font.family: root.fontFamily
        font.pixelSize: Style.font.caption
        Layout.alignment: Qt.AlignVCenter
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
      width: Math.max(0, parent.width - parent.children[0].implicitWidth - parent.children[2].implicitWidth - parent.spacing * 2)
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
