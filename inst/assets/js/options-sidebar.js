// The board options sidebar (options_sidebar_ui() in R): a list of option
// categories, each row opening that category's page. While a page is open
// the sidebar carries `.blockr-sidebar-paged`, which shows the back arrow
// sidebar_ui() draws, and its title names the category. The arrow or Escape
// returns to the list, and so does closing the sidebar, so it always opens
// on the list.
(function () {
  'use strict';

  // The Escape layer of each sidebar showing a page.
  var layers = new WeakMap();

  function openPage(root) {
    return root.querySelector('.blockr-options-page:not([hidden])');
  }

  function showPage(root, category) {
    var sidebar = root.closest('.blockr-sidebar');
    var page = null;
    root.querySelectorAll('.blockr-options-page').forEach(function (pg) {
      pg.hidden = pg.getAttribute('data-category') !== category;
      if (!pg.hidden) page = pg;
    });
    if (!page) return;
    root.querySelector('.blockr-options-list').hidden = true;
    var title = sidebar.querySelector('.blockr-sidebar-title');
    title.setAttribute('data-list-title', title.textContent);
    title.textContent = page.getAttribute('data-label') || category;
    sidebar.classList.add('blockr-sidebar-paged');
    layers.set(sidebar, Blockr.layer(sidebar, {
      inPage: true,
      escape: function () { showList(root, true); }
    }));
    // Shiny outputs inside a page that was hidden at render wake up on show.
    $(page).trigger('shown');
    // A page that wants to know it opened names an input: the code page asks
    // for the board to be built this way.
    var ping = page.querySelector('[data-open-input]');
    if (ping) {
      Shiny.setInputValue(ping.getAttribute('data-open-input'), Date.now(),
                          { priority: 'event' });
    }
    var first = page.querySelector('input, select, textarea, button');
    if (first) first.focus({ preventScroll: true });
  }

  // `refocus` hands the focus to the row of the page that closes, which a
  // way back taken from the keyboard or the arrow needs: the page held it.
  function showList(root, refocus) {
    var sidebar = root.closest('.blockr-sidebar');
    var page = openPage(root);
    if (!page) return;
    var layer = layers.get(sidebar);
    if (layer) layer.remove();
    layers.delete(sidebar);
    page.hidden = true;
    root.querySelector('.blockr-options-list').hidden = false;
    var title = sidebar.querySelector('.blockr-sidebar-title');
    title.textContent = title.getAttribute('data-list-title');
    sidebar.classList.remove('blockr-sidebar-paged');
    if (refocus) {
      var category = page.getAttribute('data-category');
      root.querySelectorAll('.blockr-options-row').forEach(function (row) {
        if (row.getAttribute('data-category') === category) row.focus();
      });
    }
  }

  document.addEventListener('click', function (e) {
    if (!(e.target instanceof Element)) return;
    var row = e.target.closest('.blockr-options-row');
    if (row) {
      e.preventDefault();
      showPage(row.closest('.blockr-options'), row.getAttribute('data-category'));
      return;
    }
    var back = e.target.closest('.blockr-sidebar-back');
    var root = back && back.closest('.blockr-sidebar').querySelector('.blockr-options');
    if (root) {
      e.preventDefault();
      showList(root, true);
    }
  });

  // The sidebar reports every open, close and pin as this event on its panel.
  // It does not bubble, so it is caught on the way down.
  document.addEventListener('blockr-sidebar:state', function (e) {
    var sidebar = e.target;
    if (sidebar.classList.contains('blockr-sidebar-open')) return;
    var root = sidebar.querySelector('.blockr-options');
    if (root) showList(root, false);
  }, true);
})();
