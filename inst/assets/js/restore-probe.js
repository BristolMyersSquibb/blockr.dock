$(function () {
  // Instrumentation for #473, where a rail renders at the floor dockView puts
  // under an edge group -- about `collapsed_size + 50px`, which is roughly 92px
  // for the 35px default -- on a viewport with several times the room its
  // declared width needs. The squeeze is permanent once it happens: a rail is
  // the low-priority view of the shell splitview, and the space a widening dock
  // regains is handed to the high-priority centre instead, so nothing revisits
  // the width. That much is understood. What is not is why the layout wanted
  // the rail that narrow in the first place, on a dock that had the room.
  //
  // Two candidates fit, and the dock container's width at the moment `fromJSON`
  // runs separates them. A container already at its full width leaves an
  // unseeded grid as the only reading: dockView sizes its grid from a
  // ResizeObserver, and a rail created before that seed arrives takes a
  // proportional share of nothing rather than the pixels it asked for. A
  // container measuring narrow says the opposite -- the restore fired ahead of
  // the layout, and the rail was squeezed by a dock that really was that small.
  // Those want different fixes and there is no evidence to choose between them,
  // so this records rather than corrects.
  //
  // Reading here and reporting in R is not a detour. The two quantities that
  // separate the candidates are the container's width and dockView's own grid
  // size at one instant, and neither is anything the server holds -- it could
  // ask, but the answer would come back a round trip later, after the layout
  // has settled and the state worth reading is gone. So the read has to happen
  // in the browser. Where it is *shown* is free, and a console the developer is
  // already watching beats a value that has to be known about to be found, so
  // each read goes back as a Shiny input for `format_restore_probe()` to write
  // out. Nothing here loads unless debug logging is on: `board_ui()` attaches
  // the dependency only then, which is the same gate the server sends under.
  //
  // The read runs on a message the server sends immediately ahead of
  // `restore_dock()`, so this handler runs in the same batch, before the
  // restore and with nothing in between that could resize anything. That first
  // read is therefore the state `fromJSON` itself sees.
  //
  // What the restore produces is read back twice more, and neither read is on a
  // fixed delay from the message. Measured, the rails do not exist two frames
  // after the handler returns -- a fixed read there sees the pre-restore dock
  // and duplicates the first line for nothing. So the second read polls by
  // animation frame until a rail appears and reports what it was *born* at,
  // and the third follows half a second after that. A rail wrong when born and
  // right when settled was corrected by the seed that sizes the grid; one wrong
  // in both never recovers, which is the reported shape.
  //
  // The poll is bounded, and reports on giving up rather than falling silent: a
  // restore that never produces a rail is itself a finding, and a probe that
  // says nothing would be read as a probe that did not run.
  var SETTLE_MS = 500;

  var MAX_FRAMES = 120;

  var dockApi = function (id) {
    var widget = HTMLWidgets.find('#' + CSS.escape(id));

    return widget && typeof widget.getWidget === 'function'
      ? widget.getWidget()
      : null;
  };

  var boxOf = function (el) {
    if (!el) return null;

    var rect = el.getBoundingClientRect();

    return { width: rect.width, height: rect.height };
  };

  var edgeGroup = function (api, position) {
    return api.groups.filter(function (group) {
      var loc = group.api.location;
      return loc && loc.type === 'edge' && loc.position === position;
    })[0];
  };

  // Absent rather than zero-width is the distinction that matters in the first
  // read: before the restore the rails a fresh dock is about to be given do not
  // exist yet, and a `null` says so rather than implying a measurement.
  var railsOf = function (api, positions) {
    if (!api) return null;

    var out = {};

    positions.forEach(function (position) {
      var group = edgeGroup(api, position);

      out[position] = group ? {
        width: boxOf(group.element).width,
        panels: group.panels.length,
        visible: api.isEdgeGroupVisible(position),
        collapsed: group.api.isCollapsed()
      } : null;
    });

    return out;
  };

  // Each read is its own event, so the three arrive as three log lines in the
  // order they were taken. An `event` priority is what makes the second and
  // third arrive at all: a settled dock reports the same geometry twice, and
  // Shiny drops an input value identical to the last one.
  var report = function (when, m) {
    var api = dockApi(m.id);

    Shiny.setInputValue(
      m.id + '_restore-probe',
      {
        id: m.id,
        at: when,
        viewport: window.innerWidth,
        container: boxOf(document.getElementById(m.id)),
        dock: api ? { width: api.width, height: api.height } : null,
        rails: railsOf(api, Object.keys(m.asked || {})),
        asked: m.asked
      },
      { priority: 'event' }
    );
  };

  var born = function (m) {
    var rails = railsOf(dockApi(m.id), Object.keys(m.asked || {}));

    return !!rails && Object.keys(rails).some(function (position) {
      return !!rails[position];
    });
  };

  var awaitRails = function (m, frames) {
    if (!born(m) && frames > 0) {
      requestAnimationFrame(function () {
        awaitRails(m, frames - 1);
      });
      return;
    }

    report('created', m);

    setTimeout(function () {
      report('settled', m);
    }, SETTLE_MS);
  };

  Shiny.addCustomMessageHandler('blockr-dock-restore-probe', function (m) {
    report('restore', m);
    awaitRails(m, MAX_FRAMES);
  });
});
