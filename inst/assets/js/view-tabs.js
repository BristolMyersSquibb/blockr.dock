// Views as tabs: a second navbar line, the board option `view_tabs` (see
// view-tabs.R). The option shows as `.blockr-view-tabs` on <html>: an inline
// script sets it before the navbar paints, and the option's server pushes
// every change ('blockr-view-tabs'). "Show views as tabs" in the views menu
// clicks the option's switch in the options sidebar, so the menu and the
// sidebar set the same option.
//
// The tabs mirror the views menu (`.blockr-view-nav`): one tab per
// `.blockr-view-item`, the active one marked. A click on a tab clicks the
// menu's own item, so a switch goes through the view binding exactly as a
// pick in the menu does, and the menu stays the one input the server hears.
// A MutationObserver on the menu keeps the tabs in step with adds, removes,
// renames, reorders and the server moving the active view.
(function () {

  var root = document.documentElement;

  var navbar = function () {
    return document.querySelector('.blockr-navbar');
  };

  var viewNav = function () {
    var bar = navbar();
    return bar ? bar.querySelector('.blockr-view-nav') : null;
  };

  var isOn = function () {
    return root.classList.contains('blockr-view-tabs');
  };

  var render = function () {
    var bar = navbar();
    var nav = viewNav();
    if (!bar || !nav) return;
    var row = bar.querySelector('.blockr-view-tabs');
    if (!row) return;

    var items = nav.querySelectorAll('.blockr-view-item');
    var frag = document.createDocumentFragment();
    items.forEach(function (item) {
      var name = item.querySelector('.blockr-view-item-name');
      var tab = document.createElement('button');
      var on = item.classList.contains('active');
      tab.type = 'button';
      tab.className = 'blockr-view-tab' + (on ? ' is-active' : '');
      tab.setAttribute('role', 'tab');
      tab.setAttribute('aria-selected', on ? 'true' : 'false');
      tab.dataset.viewId = item.dataset.viewId;
      tab.textContent = name ? name.textContent : '';
      frag.appendChild(tab);
    });
    row.replaceChildren(frag);

    var active = row.querySelector('.is-active');
    if (active && isOn()) {
      active.scrollIntoView({ block: 'nearest', inline: 'nearest' });
    }
  };

  var syncToggle = function () {
    document.querySelectorAll('.blockr-view-tabs-toggle').forEach(function (b) {
      b.setAttribute('aria-checked', isOn() ? 'true' : 'false');
    });
  };

  // The view container is sized against the navbar's height; tell it when
  // the second line comes or goes, and let dockview re-measure.
  var syncHeight = function () {
    var bar = navbar();
    if (!bar) return;
    root.style.setProperty('--blockr-navbar-height', bar.offsetHeight + 'px');
    window.dispatchEvent(new Event('resize'));
  };

  var setOn = function (on) {
    root.classList.toggle('blockr-view-tabs', on);
    syncToggle();
    render();
    syncHeight();
  };

  // The option's switch; a board saved before the option existed has none,
  // and then the menu has no row for it either.
  var optionSwitch = function () {
    return document.querySelector('.blockr-view-tabs-option input[type="checkbox"]');
  };

  var init = function () {
    var bar = navbar();
    var nav = viewNav();
    if (!bar || !nav) return;

    render();
    syncToggle();
    syncHeight();

    if (!optionSwitch()) {
      document.querySelectorAll('.blockr-view-tabs-toggle').forEach(function (b) {
        b.hidden = true;
      });
    }

    new MutationObserver(render).observe(nav, {
      subtree: true,
      childList: true,
      characterData: true,
      attributes: true,
      attributeFilter: ['class']
    });

    bar.addEventListener('click', function (e) {
      if (e.target.closest('.blockr-view-tabs-toggle')) {
        var sw = optionSwitch();
        if (sw) sw.click();
        return;
      }
      var tab = e.target.closest('.blockr-view-tab');
      if (!tab) return;
      var item = viewNav().querySelector(
        '.blockr-view-item[data-view-id="' + CSS.escape(tab.dataset.viewId) + '"]'
      );
      // jQuery's trigger, so the view binding's delegated handler runs
      if (item) $(item).trigger('click');
    });
  };

  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('blockr-view-tabs', function (on) {
      setOn(on === true);
    });
  });

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
