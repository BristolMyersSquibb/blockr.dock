// Opens a block's "…" menu with Blockr.menu (blockr.ui). The trigger carries
// the menu as JSON in `data-blockr-menu` (block_card_dropdown() in R); an
// item's `action` becomes its pick:
//   section toggle `section` on the card's section input (`target`,
//           section-toggle.js), checked while the section is open
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
    if (item.action === 'section') {
      return function () {
        var el = document.getElementById(target);
        if (el) {
          el.dispatchEvent(
            new CustomEvent('blockr-section:toggle', { detail: item.section })
          );
        }
      };
    }
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

  function isOpen(item) {
    var el = document.getElementById(item.target);
    return !!el &&
      el.getAttribute('data-sections').split(' ').indexOf(item.section) >= 0;
  }

  function config(trigger) {
    var cfg = JSON.parse(trigger.getAttribute('data-blockr-menu'));
    cfg.items.forEach(function (item) {
      if (item.action === 'section') item.checked = isOpen(item);
      if (item.action) item.onSelect = onSelect(item);
    });
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
