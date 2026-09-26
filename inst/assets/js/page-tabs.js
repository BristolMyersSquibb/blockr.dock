// The page tab row (board option `page_nav` = "tabs").
//
// The tabs mirror the page menu (`.blockr-view-nav`): one tab per
// `.blockr-view-item`, the active one marked. A click on a tab clicks the
// menu's own item, so a switch goes through the view binding exactly as a
// pick in the menu does, and the menu stays the one input the server hears.
// A MutationObserver on the menu keeps the tabs in step with adds, removes,
// renames, reorders and the server moving the active page.
(function () {

  var navbar = function () {
    return document.querySelector('.blockr-navbar');
  };

  var pageNav = function () {
    var bar = navbar();
    return bar ? bar.querySelector('.blockr-view-nav') : null;
  };

  var render = function () {
    var bar = navbar();
    var nav = pageNav();
    if (!bar || !nav) return;
    var row = bar.querySelector('.blockr-page-tabs');
    if (!row) return;

    var items = nav.querySelectorAll('.blockr-view-item');
    var frag = document.createDocumentFragment();
    items.forEach(function (item) {
      var name = item.querySelector('.blockr-view-item-name');
      var tab = document.createElement('button');
      var on = item.classList.contains('active');
      tab.type = 'button';
      tab.className = 'blockr-page-tab' + (on ? ' is-active' : '');
      tab.setAttribute('role', 'tab');
      tab.setAttribute('aria-selected', on ? 'true' : 'false');
      tab.dataset.viewId = item.dataset.viewId;
      tab.textContent = name ? name.textContent : '';
      frag.appendChild(tab);
    });
    row.replaceChildren(frag);

    var active = row.querySelector('.is-active');
    if (active && bar.dataset.pageNav === 'tabs') {
      active.scrollIntoView({ block: 'nearest', inline: 'nearest' });
    }
  };

  // The view container is sized against the navbar's height; tell it when
  // the second row comes or goes, and let dockview re-measure.
  var syncHeight = function () {
    var bar = navbar();
    if (!bar) return;
    document.documentElement.style.setProperty(
      "--blockr-navbar-height", bar.offsetHeight + 'px'
    );
    window.dispatchEvent(new Event('resize'));
  };

  var setMode = function (mode) {
    var bar = navbar();
    if (!bar) return;
    bar.dataset.pageNav = mode === 'tabs' ? 'tabs' : 'path';
    render();
    syncHeight();
  };

  var init = function () {
    var bar = navbar();
    var nav = pageNav();
    if (!bar || !nav) return;

    render();
    syncHeight();

    new MutationObserver(render).observe(nav, {
      subtree: true,
      childList: true,
      characterData: true,
      attributes: true,
      attributeFilter: ['class']
    });

    bar.addEventListener('click', function (e) {
      var tab = e.target.closest('.blockr-page-tab');
      if (!tab) return;
      var item = pageNav().querySelector(
        '.blockr-view-item[data-view-id="' + CSS.escape(tab.dataset.viewId) + '"]'
      );
      // jQuery's trigger, so the view binding's delegated handler runs
      if (item) $(item).trigger('click');
    });
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  $(document).on('shiny:connected', function () {
    Shiny.addCustomMessageHandler('blockr-page-nav', function (msg) {
      setMode(msg.mode);
    });
  });
})();
