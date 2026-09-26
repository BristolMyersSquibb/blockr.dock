// The "+" menu (add-block-menu.R). The add, append, prepend and insert
// actions run on the server, so the menu opens on a message; it opens where
// the gesture happened: at the button that was clicked if it is still on
// screen, else at the point of the last click or right-click (a context
// menu's item, say, which is gone by now), else near the top of the page.
// A pick sends the block type as the browser's commit, with no ids, so the
// server builds the block as it always has.
(function () {
  var last = null;
  var nonce = 0;

  function remember(e) {
    var el = e.target instanceof Element
      ? e.target.closest('button, a, [role="button"], [role="menuitem"]')
      : null;
    last = { x: e.clientX, y: e.clientY, el: el };
  }
  document.addEventListener('pointerdown', remember, true);
  document.addEventListener('contextmenu', remember, true);

  function visible(el) {
    if (!el || !el.isConnected) return false;
    var r = el.getBoundingClientRect();
    return r.width > 0 && r.height > 0;
  }

  // A zero-size box to hang the menu from when the trigger is gone.
  function pointAnchor(x, y) {
    var a = document.createElement('div');
    a.className = 'blockr-add-menu-anchor';
    a.style.cssText = 'position:fixed;width:0;height:0;left:' + x + 'px;top:' + y + 'px;';
    document.body.appendChild(a);
    return a;
  }

  function anchorFor() {
    if (last && visible(last.el) && !last.el.closest('.blockr-menu')) {
      return { anchor: last.el, temp: null };
    }
    var a = last
      ? pointAnchor(last.x, last.y)
      : pointAnchor(Math.round(window.innerWidth / 2) - 150, 64);
    return { anchor: a, temp: a };
  }

  function open(m) {
    if (!window.Blockr || !Blockr.menu) return;
    var at = anchorFor();
    var anchor = at.anchor, temp = at.temp;

    var items = (m.items || []).map(function (it) {
      if (!it.type) return it;
      var type = it.type;
      return Object.assign({}, it, {
        onSelect: function () {
          Shiny.setInputValue(m.commit, {
            type: type,
            id: null,
            title: null,
            link_id: null,
            near_link_id: null,
            far_link_id: null,
            block_input: null,
            target_input: null,
            nonce: ++nonce
          }, { priority: 'event' });
        }
      });
    });

    Blockr.menu(anchor, {
      caption: m.caption,
      filter: 'Search blocks',
      minWidth: 300,
      items: items,
      onClose: function () {
        if (temp) temp.remove();
      }
    });
  }

  // Adding a panel to the page: the board's blocks and extensions that are
  // not on it yet; a pick sends the panel id.
  function openPanels(m) {
    if (!window.Blockr || !Blockr.menu) return;
    var at = anchorFor();
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

  if (window.Shiny) {
    Shiny.addCustomMessageHandler('blockr-add-block-menu', open);
    Shiny.addCustomMessageHandler('blockr-add-panel-menu', openPanels);
  }
})();
