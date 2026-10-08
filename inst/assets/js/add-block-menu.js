// The "+" menu (add-block-menu.R). The add, append, prepend and insert
// actions run on the server, so the menu opens on a message, at what the
// action's trigger named (`at`): an element while it is on screen, or a
// point. Without either, as for an action fired from code, it opens near the
// top of the page. A pick sends the block type as the browser's commit; the
// server generates the block's id and resolves the link's port. The chevron
// at a row's end, under the pointer, opens the block's options before it is
// added.
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

  // The fixed spot centres a menu of the "+" menu's width.
  var WIDTH = 300;

  function anchorFor(at, width) {
    var el = at && at.id ? document.getElementById(at.id) : null;
    if (visible(el)) return { anchor: el, temp: null };
    var a = at && at.x != null
      ? pointAnchor(at.x, at.y)
      : pointAnchor(Math.round((window.innerWidth - (width || 300)) / 2), 64);
    return { anchor: a, temp: a };
  }

  function commit(m, spec) {
    Shiny.setInputValue(m.commit, Object.assign({}, spec, { nonce: ++nonce }), {
      priority: 'event'
    });
  }

  var VERB = { add: 'Add', append: 'Append', prepend: 'Prepend', insert: 'Insert' };

  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text != null) e.textContent = text;
    return e;
  }

  // A labelled text field of the options form.
  function textField(form, label, opts) {
    var wrap = el('div', 'blockr-add-options__field');
    var lab = el('label', 'blockr-add-options__label', label);
    var input = el('input', 'blockr-add-options__input' + (opts.mono ? ' is-mono' : ''));
    input.type = 'text';
    input.placeholder = opts.placeholder || '';
    input.id = 'blockr-add-opt-' + (++nonce);
    lab.htmlFor = input.id;
    var err = el('div', 'blockr-add-options__error');
    err.hidden = true;
    wrap.appendChild(lab);
    wrap.appendChild(input);
    wrap.appendChild(err);
    form.appendChild(wrap);
    return {
      input: input,
      value: function () { return input.value.trim(); },
      fail: function (msg) {
        err.hidden = !msg;
        err.textContent = msg || '';
        wrap.classList.toggle('is-bad', !!msg);
        return !msg;
      }
    };
  }

  // A choice among a few inputs, as a segmented control; the first is on.
  function choiceField(form, label, choices) {
    var wrap = el('div', 'blockr-add-options__field');
    wrap.appendChild(el('div', 'blockr-add-options__label', label));
    var seg = el('div', 'blockr-segmented blockr-segmented--xs');
    seg.setAttribute('role', 'radiogroup');
    seg.setAttribute('aria-label', label);
    var value = choices[0];
    choices.forEach(function (c, i) {
      var b = el('button', 'blockr-segmented__seg' + (i === 0 ? ' is-selected' : ''), c);
      b.type = 'button';
      b.setAttribute('role', 'radio');
      b.setAttribute('aria-checked', i === 0 ? 'true' : 'false');
      b.addEventListener('click', function () {
        value = c;
        seg.querySelectorAll('.blockr-segmented__seg').forEach(function (x) {
          var on = x === b;
          x.classList.toggle('is-selected', on);
          x.setAttribute('aria-checked', on ? 'true' : 'false');
        });
      });
      seg.appendChild(b);
    });
    wrap.appendChild(seg);
    form.appendChild(wrap);
    return { value: function () { return value; } };
  }

  // The options of one block before it is added: its ID and title, the input
  // where there is a choice, and the ids of the links the flow makes. Enter
  // or the button adds it; Escape or the arrow goes back to the list, with
  // its search. Empty fields keep the defaults: a generated ID, the block's
  // own name, the first free input.
  function openOptions(m, it, query) {
    var at = anchorFor(m.at, WIDTH);
    var opt = it.options || {};
    var mode = m.mode || 'add';
    var taken = (m.taken_ids || []).map(String);
    var takenLinks = (m.taken_links || []).map(String);
    var back = false;

    var handle = Blockr.menu(at.anchor, {
      minWidth: WIDTH,
      items: [],
      onClose: function (how) {
        if (at.temp) at.temp.remove();
        if (how === 'escape' || back) open(m, query);
      }
    });

    var panel = handle.el;
    // A form, not a list: it takes the height it needs, short of the screen.
    panel.style.maxHeight = 'calc(100vh - 32px)';
    var head = el('div', 'blockr-add-options__head');
    var backBtn = el('button', 'blockr-add-options__back');
    backBtn.type = 'button';
    backBtn.setAttribute('aria-label', 'Back to the list');
    backBtn.innerHTML = Blockr.icons.chevron;
    backBtn.addEventListener('click', function () { back = true; handle.close(); });
    head.appendChild(backBtn);
    var mark = el('span', 'blockr-block-mark');
    if (it.mark && it.mark.category) mark.dataset.category = it.mark.category;
    mark.innerHTML = (it.mark && it.mark.icon) || '';
    head.appendChild(mark);
    head.appendChild(el('span', 'blockr-add-options__name', it.label));
    panel.insertBefore(head, panel.firstChild);

    var form = el('form', 'blockr-add-options');
    if (opt.description) form.appendChild(el('p', 'blockr-add-options__desc', opt.description));

    var id = textField(form, 'ID', { placeholder: 'generated', mono: true });
    var title = textField(form, 'Title', { placeholder: it.label });

    var input = null;
    var inputKey = null;
    if (mode === 'append' || mode === 'insert') {
      inputKey = 'block_input';
      var ins = (opt.inputs || []).map(String);
      if (opt.variadic) {
        input = textField(form, 'Input name', { placeholder: 'unnamed' });
      } else if (ins.length > 1) {
        input = choiceField(form, 'The link goes into', ins);
      }
    } else if (mode === 'prepend') {
      inputKey = 'target_input';
      var free = (m.target_inputs || []).map(String);
      if (m.target_variadic) {
        input = textField(form, 'Input name', { placeholder: 'unnamed' });
      } else if (free.length > 1) {
        input = choiceField(form, 'It goes into', free);
      }
    }

    var link = null, near = null, far = null;
    if (mode === 'append' || mode === 'prepend') {
      link = textField(form, 'Link ID', { placeholder: 'generated', mono: true });
    } else if (mode === 'insert') {
      near = textField(form, 'Incoming link ID', { placeholder: 'generated', mono: true });
      far = textField(form, 'Outgoing link ID', { placeholder: 'generated', mono: true });
    }

    // The design system's main button (the accent tint) at size s, at the
    // right, as a dialog places it.
    var end = el('div', 'blockr-add-options__end');
    var submit = el('button', 'blockr-btn blockr-btn--main blockr-btn--s', VERB[mode] || 'Add');
    submit.type = 'submit';
    end.appendChild(submit);
    form.appendChild(end);

    function check() {
      var ok = true;
      if (id.value() && taken.indexOf(id.value()) >= 0) ok = id.fail('A block has this ID') && ok; else id.fail(null);
      [link, near, far].forEach(function (f) {
        if (!f) return;
        if (f.value() && takenLinks.indexOf(f.value()) >= 0) ok = f.fail('A link has this ID') && ok; else f.fail(null);
      });
      if (near && far && near.value() && near.value() === far.value()) ok = far.fail('The two link IDs must differ') && ok;
      if (input && input.fail && input.value() && mode === 'prepend' &&
          (m.target_taken || []).map(String).indexOf(input.value()) >= 0) {
        ok = input.fail('An input has this name') && ok;
      }
      return ok;
    }

    form.addEventListener('input', function () { check(); });
    // The menu reads Enter, Space and Tab for its rows; inside the form they
    // are the form's (Escape still goes to the menu, which goes back).
    form.addEventListener('keydown', function (e) {
      if (e.key === 'Escape') return;
      e.stopPropagation();
      if (e.key === 'Enter' && e.target instanceof HTMLInputElement) {
        e.preventDefault();
        form.requestSubmit();
      }
    });
    form.addEventListener('submit', function (e) {
      e.preventDefault();
      if (!check()) return;
      var spec = { type: it.type };
      if (id.value()) spec.id = id.value();
      if (title.value()) spec.title = title.value();
      if (input && input.value()) spec[inputKey] = input.value();
      if (link && link.value()) spec.link_id = link.value();
      if (near && near.value()) spec.near_link_id = near.value();
      if (far && far.value()) spec.far_link_id = far.value();
      handle.close();
      commit(m, spec);
    });

    panel.insertBefore(form, panel.querySelector('.blockr-menu__list'));
    id.input.focus();
  }

  function open(m, query) {
    var at = anchorFor(m.at, WIDTH);
    var handle = null;

    // A row commits its block type; the chevron at a row's end opens its
    // options first.
    var items = (m.items || []).map(function (it) {
      if (!it.type) return it;
      var row = Object.assign({}, it, {
        onSelect: function () { commit(m, { type: it.type }); }
      });
      if (it.type) {
        row.tool = {
          label: 'Options',
          onSelect: function () {
            var box = handle && handle.el.querySelector('.blockr-menu__filter-input');
            openOptions(m, it, box ? box.value : '');
          }
        };
      }
      return row;
    });

    handle = Blockr.menu(at.anchor, {
      caption: m.caption,
      filter: 'Search blocks',
      minWidth: WIDTH,
      items: items,
      onClose: function () {
        if (at.temp) at.temp.remove();
      }
    });

    // Back from the options: the search the list had.
    var box = handle.el.querySelector('.blockr-menu__filter-input');
    if (box && query) {
      box.value = query;
      box.dispatchEvent(new Event('input', { bubbles: true }));
    }
  }

  // Adding panels to the page: the board's blocks and extensions that are
  // not on it yet, ticked and sent together as the menu closes (not on
  // Escape).
  function openPanels(m) {
    var at = anchorFor(m.at);
    Blockr.menu(at.anchor, {
      caption: m.caption,
      filter: (m.items || []).length > 8 ? 'Search' : false,
      minWidth: 260,
      multi: true,
      items: m.items || [],
      onChange: function (picked) {
        Shiny.setInputValue(m.pick, {
          values: picked.map(function (it) { return it.value; }),
          nonce: ++nonce
        }, { priority: 'event' });
      },
      onClose: function () {
        if (at.temp) at.temp.remove();
      }
    });
  }

  // Shiny takes a handler of one argument only.
  Shiny.addCustomMessageHandler('blockr-add-block-menu', function (m) { open(m); });
  Shiny.addCustomMessageHandler('blockr-add-panel-menu', openPanels);
})();
