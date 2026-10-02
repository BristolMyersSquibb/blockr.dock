// The "+" menu (add-block-menu.R). The add, append, prepend and insert
// actions run on the server, so the menu opens on a message, at what the
// action's trigger named (`at`): an element while it is on screen, or a
// point. Without either, as for an action fired from code, it opens near the
// top of the page. A pick sends the block type as the browser's commit; the
// server generates the block's id and resolves the link's port.
(function () {
  var nonce = 0;

  function visible(el) {
    if (!el || !el.isConnected) return false;
    var r = el.getBoundingClientRect();
    return r.width > 0 && r.height > 0;
  }

  // A zero-size box to hang the menu from a point.
  function pointAnchor(x, y) {
    var a = document.createElement('div');
    a.className = 'blockr-add-menu-anchor';
    a.style.cssText = 'position:fixed;width:0;height:0;left:' + x + 'px;top:' + y + 'px;';
    document.body.appendChild(a);
    return a;
  }

  function anchorFor(at) {
    var el = at && at.id ? document.getElementById(at.id) : null;
    if (visible(el)) return { anchor: el, temp: null };
    var a = at && at.x != null
      ? pointAnchor(at.x, at.y)
      : pointAnchor(Math.round(window.innerWidth / 2) - 150, 64);
    return { anchor: a, temp: a };
  }

  function open(m) {
    var at = anchorFor(m.at);

    var items = (m.items || []).map(function (it) {
      if (!it.type) return it;
      var type = it.type;
      return Object.assign({}, it, {
        onSelect: function () {
          Shiny.setInputValue(m.commit, { type: type, nonce: ++nonce }, {
            priority: 'event'
          });
        }
      });
    });

    Blockr.menu(at.anchor, {
      caption: m.caption,
      filter: 'Search blocks',
      minWidth: 300,
      items: items,
      onClose: function () {
        if (at.temp) at.temp.remove();
      }
    });
  }

  // Adding a panel to the page: the board's blocks and extensions that are
  // not on it yet; a pick sends the panel id.
  function openPanels(m) {
    var at = anchorFor(m.at);
    var items = (m.items || []).map(function (it) {
      if (!it.value) return it;
      var value = it.value;
      return Object.assign({}, it, {
        onSelect: function () {
          Shiny.setInputValue(m.pick, { value: value, nonce: ++nonce },
                              { priority: 'event' });
        }
      });
    });
    Blockr.menu(at.anchor, {
      caption: m.caption,
      filter: items.length > 8 ? 'Search' : false,
      minWidth: 260,
      items: items,
      onClose: function () {
        if (at.temp) at.temp.remove();
      }
    });
  }

  Shiny.addCustomMessageHandler('blockr-add-block-menu', open);
  Shiny.addCustomMessageHandler('blockr-add-panel-menu', openPanels);
})();
