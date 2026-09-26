// Opens a block's "…" menu with Blockr.menu (blockr.ui). The trigger carries
// the menu as JSON in `data-blockr-menu` (block_card_dropdown() in R); an
// item's `action` becomes its pick:
//   input   send `target` as a Shiny event, as the old dropdown's action
//           buttons did, so the server's observeEvent()s are unchanged
//   rename  start the in-place rename of the title (`target` is its display)
//   copy    put `target`, the block ID, on the clipboard
// Cards come and go with the dock, so the triggers are handled from the
// document rather than bound one by one. A click on the open menu's trigger
// closes it.
(function () {
  var current = null;

  function onSelect(item) {
    var target = item.target;
    if (item.action === 'input') {
      return function () {
        Shiny.setInputValue(target, Date.now(), { priority: 'event' });
      };
    }
    if (item.action === 'rename') {
      return function () {
        var el = document.getElementById(target);
        if (el) el.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
      };
    }
    if (item.action === 'copy') {
      return function () {
        if (navigator.clipboard) navigator.clipboard.writeText(target);
      };
    }
    return undefined;
  }

  // The section toggles (controls, preview, a block's own control such as
  // the AI assistant) are the menu's first group: each row with the toggle's
  // icon and a check when on. Their buttons are hidden (blockr-dock.css);
  // a pick flips the same checkbox the button would.
  function toggleItems(trigger) {
    var header = trigger.closest('.blockr-block-header');
    var group = header && header.querySelector('.blockr-section-toggle');
    if (!group) return [];
    var items = [];
    group.querySelectorAll('input[type="checkbox"]').forEach(function (input) {
      var label = group.querySelector('label[for="' + CSS.escape(input.id) + '"]') ||
        input.nextElementSibling;
      var name = label && (label.getAttribute('title') ||
        label.getAttribute('data-blockr-tip') ||
        label.getAttribute('aria-label') || label.textContent.trim());
      items.push({
        label: name || 'Control',
        icon: label ? label.innerHTML : undefined,
        checked: input.checked,
        onSelect: function () { input.click(); }
      });
    });
    return items.length ? items.concat([{ divider: true }]) : [];
  }

  function config(trigger) {
    var cfg = JSON.parse(trigger.getAttribute('data-blockr-menu'));
    cfg.items = toggleItems(trigger).concat((cfg.items || []).map(function (item) {
      if (item.action) item.onSelect = onSelect(item);
      return item;
    }));
    cfg.onClose = function () {
      if (current && current.trigger === trigger) current = null;
    };
    return cfg;
  }

  function toggle(trigger, keyboard) {
    if (current && current.trigger === trigger) {
      current.handle.close();
      return;
    }
    var handle = Blockr.menu(trigger, config(trigger));
    current = { trigger: trigger, handle: handle };
    if (keyboard) {
      handle.el.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown' }));
    }
  }

  function triggerOf(e) {
    return e.target instanceof Element ? e.target.closest('.blockr-block-menu-btn') : null;
  }

  document.addEventListener('click', function (e) {
    var t = triggerOf(e);
    if (!t) return;
    e.preventDefault();
    toggle(t, false);
  });

  document.addEventListener('keydown', function (e) {
    if (e.key !== 'ArrowDown') return;
    var t = triggerOf(e);
    if (!t) return;
    e.preventDefault();
    toggle(t, true);
  });
})();
