// The menu for adding a panel (panel-menu.R): the board's blocks and
// extensions not in the view yet. It opens on a message, at what the server
// names (`at`): an element while it is on screen, or a point. Without either
// it opens near the top of the page. A pick sends the panel id.
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
    a.className = 'blockr-panel-menu-anchor';
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

  Shiny.addCustomMessageHandler('blockr-add-panel-menu', openPanels);
})();
