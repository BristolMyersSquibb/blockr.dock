// The menus of the link and stack actions (action-menu.R). The server sends
// a menu; it opens as a Blockr.menu at what the action's trigger named
// (`at`): an element while it is on screen, or a point. A pick goes back on
// the menu's `pick` input:
//   { value }    a row
//   { values }   a multi menu's ticks, once, as it closes (not on Escape)
//   { field }    a name field, on Enter or a click elsewhere
// A menu opened from another one carries it as `back`, and Escape reopens it.
(function () {
  var nonce = 0;

  function visible(el) {
    if (!el || !el.isConnected) return false;
    var r = el.getBoundingClientRect();
    return r.width > 0 && r.height > 0;
  }

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

  function send(m, payload) {
    payload.nonce = ++nonce;
    Shiny.setInputValue(m.pick, payload, { priority: 'event' });
  }

  // A colour row opens the browser's picker straight from the click, which
  // a round trip to the server would no longer count as a user gesture.
  function pickColour(m, item) {
    var input = document.createElement('input');
    input.type = 'color';
    input.value = item.colour || '#000000';
    input.style.cssText = 'position:fixed;left:-100px;top:0;opacity:0;';
    document.body.appendChild(input);
    input.addEventListener('change', function () {
      send(m, { value: item.value + ':' + input.value });
      input.remove();
    });
    input.addEventListener('blur', function () {
      setTimeout(function () { input.remove(); }, 0);
    });
    input.click();
  }

  // A name field in a menu of its own: Enter or a click elsewhere commits,
  // a taken or empty name is refused in place, Escape goes back.
  function addField(m, handle) {
    var f = m.field;
    var wrap = document.createElement('div');
    wrap.className = 'blockr-action-menu-field';
    var input = document.createElement('input');
    input.type = 'text';
    input.className = 'blockr-action-menu-field__input';
    input.value = f.value || '';
    input.setAttribute('aria-label', m.caption || 'Name');
    if (f.placeholder) input.placeholder = f.placeholder;
    var err = document.createElement('div');
    err.className = 'blockr-action-menu-field__error';
    err.hidden = true;
    wrap.appendChild(input);
    wrap.appendChild(err);
    var list = handle.el.querySelector('.blockr-menu__list');
    handle.el.insertBefore(wrap, list);
    input.focus();
    input.select();

    var taken = (f.taken || []).map(String);
    function problem() {
      var v = input.value.trim();
      if (!v && !f.empty_ok) return f.empty_msg;
      if (v && taken.indexOf(v) >= 0) return f.taken_msg;
      return null;
    }
    function show() {
      var p = problem();
      err.hidden = !p;
      err.textContent = p || '';
      wrap.classList.toggle('blockr-action-menu-field--bad', !!p);
      return !p;
    }
    input.addEventListener('input', function () {
      if (!err.hidden) show();
    });
    input.addEventListener('keydown', function (e) {
      if (e.key !== 'Enter') return;
      e.preventDefault();
      e.stopPropagation();
      if (!show()) return;
      m.done = true;
      handle.close();
      if (input.value.trim() !== (f.value || '')) send(m, { field: input.value.trim() });
    });
    // Closed any other way but Escape: commit what is there, if it is valid.
    return function (how) {
      if (m.done || how === 'escape') return;
      if (!problem() && input.value.trim() !== (f.value || '')) {
        send(m, { field: input.value.trim() });
      }
    };
  }

  function open(m) {
    var at = anchorFor(m.at);
    var commitField = null;

    var items = (m.items || []).map(function (it) {
      if (it.value == null || m.multi) return it;
      return Object.assign({}, it, {
        onSelect: function () {
          if (it.colour_picker) pickColour(m, it);
          else send(m, { value: it.value });
        }
      });
    });

    var handle = Blockr.menu(at.anchor, {
      head: m.head || undefined,
      caption: m.caption || undefined,
      filter: m.filter ? 'Search' : false,
      minWidth: m.field ? 240 : 260,
      multi: !!m.multi,
      items: items,
      onChange: function (picked) {
        send(m, { values: picked.map(function (it) { return it.value; }) });
      },
      onClose: function (how) {
        if (at.temp) at.temp.remove();
        if (commitField) commitField(how);
        if (how === 'escape' && m.back) open(m.back);
      }
    });

    if (m.field) commitField = addField(m, handle);
  }

  Shiny.addCustomMessageHandler('blockr-action-menu', open);
})();
