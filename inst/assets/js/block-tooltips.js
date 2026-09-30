// Hands the dock chrome's tooltips to Blockr.tooltip (blockr.ui), the design
// system's light card. The sites are rendered in R and inserted as cards and
// sidebars come and go, so each is registered on the first hover or focus:
// an element in the block header, navbar, view menu or sidebars that carries
// a `title` (written by R or by JS, and written again when a status dot's
// label changes) or `data-blockr-tip` (with `data-blockr-tip-badge`, the
// block icon's type and package). A `title` is removed as it is taken over,
// so the browser's native box never shows; an icon-only element keeps it as
// its `aria-label`.
//
// The listeners sit on `window` in the capture phase, which runs before
// Blockr.tooltip's own on `document`, so an element is registered before
// the event that should show its card reaches it.
(function () {
  var SCOPE = '.blockr-block-header, .blockr-navbar, .blockr-view-nav, .blockr-sidebar';

  function register(e) {
    if (!window.Blockr || !Blockr.tooltip) return;
    var el = e.target instanceof Element ? e.target : null;
    while (el && !el.hasAttribute('title') && !el.hasAttribute('data-blockr-tip')) {
      el = el.parentElement;
    }
    if (!el || !el.closest(SCOPE)) return;

    var title = el.getAttribute('title');
    if (title !== null) {
      el.removeAttribute('title');
      if (title && !el.hasAttribute('aria-label') && !el.textContent.trim()) {
        el.setAttribute('aria-label', title);
      }
      el.setAttribute('data-blockr-tip', title);
    }

    var name = el.getAttribute('data-blockr-tip');
    var badge = el.getAttribute('data-blockr-tip-badge');
    if (!name) {
      Blockr.tooltip.clear(el);
    } else if (badge) {
      Blockr.tooltip.set(el, { name: name, badge: badge });
    } else {
      Blockr.tooltip.set(el, name);
    }
  }

  window.addEventListener('pointerover', register, true);
  window.addEventListener('focusin', register, true);
})();
