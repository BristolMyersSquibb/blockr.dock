// The board options sidebar (options_sidebar_ui() in R): a list of option
// categories; a row opens that category's page. While a page is open the
// sidebar header shows a back arrow and the category's name; the arrow or
// Escape returns to the list, and closing the sidebar returns it there too,
// so it always opens on the list.
(function () {
  var BACK_SVG =
    '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round" ' +
    'stroke-linejoin="round" aria-hidden="true">' +
    '<path d="M10 3.5L5.5 8l4.5 4.5"></path></svg>';

  function parts(root) {
    var sidebar = root.closest('.blockr-sidebar');
    return {
      sidebar: sidebar,
      title: sidebar && sidebar.querySelector('.blockr-sidebar-title'),
      header: sidebar && sidebar.querySelector('.blockr-sidebar-header'),
      list: root.querySelector('.blockr-options-list')
    };
  }

  function backButton(p, root) {
    var btn = p.header.querySelector('.blockr-sidebar-back');
    if (btn) return btn;
    btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'blockr-sidebar-btn blockr-sidebar-back';
    btn.setAttribute('aria-label', 'Back to all options');
    btn.title = 'Back';
    btn.innerHTML = BACK_SVG;
    btn.addEventListener('click', function (e) {
      e.preventDefault();
      showList(root);
    });
    p.header.insertBefore(btn, p.header.firstChild);
    return btn;
  }

  function showPage(root, category) {
    var p = parts(root);
    if (!p.sidebar) return;
    var page = null;
    root.querySelectorAll('.blockr-options-page').forEach(function (pg) {
      var match = pg.getAttribute('data-category') === category;
      pg.hidden = !match;
      if (match) page = pg;
    });
    if (!page) return;
    p.list.hidden = true;
    if (!p.title.hasAttribute('data-list-title')) {
      p.title.setAttribute('data-list-title', p.title.textContent);
    }
    p.title.textContent = category;
    backButton(p, root);
    widen(p.sidebar, page);
    p.sidebar.classList.add('blockr-sidebar-paged');
    // Shiny outputs inside a page that was hidden at render wake up on show.
    if (window.jQuery) window.jQuery(page).trigger('shown');
    var first = page.querySelector('input, select, textarea, button');
    if (first) first.focus({ preventScroll: true });
  }

  // A page may ask for more width while it is open: an element in it with
  // `data-blockr-page-width` (px), such as blockr.theme's scale map editor.
  // The sidebar never gets narrower for it, and gets its width back on the
  // list.
  var WIDTH = '--blockr-sidebar-panel-width';
  function widen(sidebar, page) {
    var ask = page.querySelector('[data-blockr-page-width]');
    var px = ask ? parseInt(ask.getAttribute('data-blockr-page-width'), 10) : 0;
    if (!px || px <= sidebar.getBoundingClientRect().width) return;
    if (!sidebar.hasAttribute('data-list-width')) {
      sidebar.setAttribute('data-list-width', sidebar.style.getPropertyValue(WIDTH));
    }
    sidebar.style.setProperty(WIDTH, px + 'px');
  }
  function unwiden(sidebar) {
    if (!sidebar.hasAttribute('data-list-width')) return;
    var was = sidebar.getAttribute('data-list-width');
    if (was) sidebar.style.setProperty(WIDTH, was);
    else sidebar.style.removeProperty(WIDTH);
    sidebar.removeAttribute('data-list-width');
  }

  function showList(root) {
    var p = parts(root);
    if (!p.sidebar) return;
    root.querySelectorAll('.blockr-options-page').forEach(function (pg) {
      pg.hidden = true;
    });
    p.list.hidden = false;
    if (p.title && p.title.hasAttribute('data-list-title')) {
      p.title.textContent = p.title.getAttribute('data-list-title');
    }
    // Only when set: classList.remove() rewrites the class attribute even for
    // an absent class, and the watcher below would take that for a change.
    unwiden(p.sidebar);
    if (p.sidebar.classList.contains('blockr-sidebar-paged')) {
      p.sidebar.classList.remove('blockr-sidebar-paged');
    }
  }

  document.addEventListener('click', function (e) {
    var row = e.target instanceof Element ? e.target.closest('.blockr-options-row') : null;
    if (!row) return;
    e.preventDefault();
    showPage(row.closest('.blockr-options'), row.getAttribute('data-category'));
  });

  // Escape on a page goes back to the list instead of closing the sidebar.
  // Capture phase, so it runs before the sidebar's own Escape handler.
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Escape') return;
    var sidebar = e.target instanceof Element ? e.target.closest('.blockr-sidebar-paged') : null;
    if (!sidebar) return;
    var root = sidebar.querySelector('.blockr-options');
    if (!root) return;
    e.preventDefault();
    e.stopPropagation();
    showList(root);
  }, true);

  // Closing the sidebar returns it to the list.
  var watched = new WeakSet();
  function watch() {
    document.querySelectorAll('.blockr-options').forEach(function (root) {
      var sidebar = root.closest('.blockr-sidebar');
      if (!sidebar || watched.has(sidebar)) return;
      watched.add(sidebar);
      new MutationObserver(function () {
        if (!sidebar.classList.contains('blockr-sidebar-open') &&
            sidebar.classList.contains('blockr-sidebar-paged')) {
          showList(root);
        }
      }).observe(sidebar, { attributes: true, attributeFilter: ['class'] });
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', watch);
  } else {
    watch();
  }
  document.addEventListener('click', watch, true);
})();
