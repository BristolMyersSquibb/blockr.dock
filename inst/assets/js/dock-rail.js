$(function () {
  // Rails (dockview edge groups) are declared once per dock and never removed:
  // removing one disposes the panels it holds. What varies is whether a rail is
  // shown, and that is derived rather than stored -- a rail holding panels is
  // visible, an empty one is not.
  //
  // TWIN: `rail_to_edge()` in R/dock-layout.R applies the same rule when it
  // builds the restore payload. Change one and you must change the other. The
  // split is not redundancy: the server decides what the dock is born as,
  // because nothing here can correct a payload before the first paint, and
  // this half owns every moment after, because a panel leaving a rail is a
  // client gesture and a server round-trip would leave the rail standing empty
  // until the echo came back.
  //
  // The other half is reveal-on-drag. A hidden rail has no hit area, so a drag
  // toward the edge has nothing to aim at. Dragging into the band the collapsed
  // strip would occupy reveals the rail there -- collapsed, so an empty one
  // shows its strip rather than a full-width empty pane -- and the drop expands
  // it. A drag that ends anywhere else leaves the derived rule to hide it
  // again. This has to happen on the client: a server round-trip mid-drag
  // arrives long after the pointer has moved on.

  var DWELL_MS = 250;

  var docks = {};

  // The drag in flight, page-wide. Only one HTML5 drag can be active at a time,
  // so this is a single slot rather than a per-dock one.
  var drag = null;

  var edgeGroup = function (api, position) {
    return api.groups.filter(function (group) {
      var loc = group.api.location;
      return loc && loc.type === 'edge' && loc.position === position;
    })[0];
  };

  // The derived rule, plus the one thing a reveal owes the user: a rail that a
  // drag revealed came up collapsed, so expand it once a panel has landed in
  // it. Note the pair is `collapse()` / `expand()`, both zero-arity -- there is
  // no `collapse(bool)`, so a `collapse(false)` reads as a no-op rather than as
  // an expand. `setSize()` is also a no-op while collapsed.
  var sync = function (dock) {
    // A removed view takes its dock's container with it. Reclaim the entry
    // rather than driving an api whose element is gone -- views come and go
    // through the nav, so `docks` would otherwise only ever grow.
    if (!document.body.contains(dock.el)) {
      teardown(dock.id);
      return;
    }

    dock.rails.forEach(function (rail) {
      var position = rail.position;
      var group = edgeGroup(dock.api, position);

      if (!group) return;

      var wanted = group.panels.length > 0;

      // Mid-drag, a rail we revealed is deliberately empty and visible. Leave
      // it alone until the drag resolves, or we would hide the target the user
      // is currently aiming at.
      if (!wanted && dock.revealed[position] && drag) return;

      if (dock.api.isEdgeGroupVisible(position) !== wanted) {
        dock.api.setEdgeGroupVisible(position, wanted);
      }

      if (wanted && dock.revealed[position] && group.api.isCollapsed()) {
        group.api.expand();
      }

      if (!drag) dock.revealed[position] = false;
    });
  };

  var reveal = function (dock, rail) {
    var position = rail.position;

    if (dock.api.isEdgeGroupVisible(position)) return;

    dock.revealed[position] = true;
    dock.api.setEdgeGroupVisible(position, true);

    var group = edgeGroup(dock.api, position);

    if (group && !group.api.isCollapsed()) group.api.collapse();
  };

  // The hot zone is the strip's own footprint, so what the user aims at and
  // what appears are the same band.
  var inBand = function (rect, position, x, y, band) {
    if (x < rect.left || x > rect.right || y < rect.top || y > rect.bottom) {
      return false;
    }

    if (position === 'left') return x - rect.left <= band;
    if (position === 'right') return rect.right - x <= band;
    if (position === 'top') return y - rect.top <= band;
    if (position === 'bottom') return rect.bottom - y <= band;

    return false;
  };

  var clearDwell = function (dock) {
    if (dock.timer) {
      clearTimeout(dock.timer);
      dock.timer = null;
    }
    dock.pending = null;
  };

  var onDragOver = function (dock, event) {
    if (!drag) return;

    var rect = dock.el.getBoundingClientRect();
    var x = event.clientX;
    var y = event.clientY;

    var hit = dock.rails.filter(function (rail) {
      return inBand(rect, rail.position, x, y, rail.collapsedSize);
    })[0];

    if (!hit) {
      clearDwell(dock);
      return;
    }

    if (dock.pending === hit.position) return;

    // A short dwell, so sweeping past an edge on the way somewhere else does
    // not flash the rail open.
    clearDwell(dock);
    dock.pending = hit.position;
    dock.timer = setTimeout(function () {
      dock.timer = null;
      dock.pending = null;
      if (drag) reveal(dock, hit);
    }, DWELL_MS);
  };

  // Re-assert the derived rule once the gesture has resolved. A drop reaches
  // this handler by bubbling out of the group dockview handled it in, so the
  // panel has already landed and `sync()` reads the settled membership.
  var endDrag = function () {
    if (!drag) return;

    drag = null;

    Object.keys(docks).forEach(function (id) {
      var dock = docks[id];
      clearDwell(dock);
      sync(dock);
    });
  };

  var teardown = function (id) {
    var dock = docks[id];

    if (!dock) return;

    clearDwell(dock);
    dock.disposables.forEach(function (d) {
      if (d && typeof d.dispose === 'function') d.dispose();
    });
    $(dock.el).off('.blockrRail');
    delete docks[id];
  };

  var wire = function (id, rails) {
    var el = document.getElementById(id);
    var widget = el ? HTMLWidgets.find('#' + CSS.escape(id)) : null;
    var api = widget && typeof widget.getWidget === 'function'
      ? widget.getWidget()
      : null;

    if (!api) return false;

    teardown(id);

    var dock = {
      id: id,
      el: el,
      api: api,
      rails: rails,
      revealed: {},
      pending: null,
      timer: null,
      disposables: []
    };

    docks[id] = dock;

    var onChange = function () {
      sync(dock);
    };

    dock.disposables.push(
      api.onDidAddPanel(onChange),
      api.onDidRemovePanel(onChange),
      api.onDidMovePanel(onChange),
      api.onWillDragPanel(function () {
        drag = { id: id };
      }),
      api.onWillDragGroup(function () {
        drag = { id: id };
      })
    );

    $(el)
      .on('dragover.blockrRail', function (e) {
        onDragOver(dock, e.originalEvent || e);
      })
      .on('drop.blockrRail dragend.blockrRail', endDrag);

    sync(dock);

    return true;
  };

  // Escape cancels an HTML5 drag, which fires `dragend` on the source -- but
  // the source may sit in a dock whose container the pointer has long left, so
  // key off the document as well.
  $(document).on('keydown.blockrRail', function (e) {
    if (e.key === 'Escape') endDrag();
  });

  Shiny.addCustomMessageHandler('blockr-dock-rails', function (m) {
    if (wire(m.id, m.rails)) return;

    // The dock output renders in the same flush that sends this, so the widget
    // may not have been instantiated yet.
    var attempts = 0;
    var timer = setInterval(function () {
      attempts++;
      if (wire(m.id, m.rails) || attempts >= 20) clearInterval(timer);
    }, 50);
  });
});
