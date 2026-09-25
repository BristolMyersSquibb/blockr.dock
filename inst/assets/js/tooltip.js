// The design system's tooltip, a small light card, for the dock's own chrome:
// the block header, the navbar, the view menu and the sidebars. Every site
// there still writes a plain `title` attribute (from R or from JS); on the
// first hover or focus this script moves it to `data-blockr-tip`, so the
// browser's native box never shows, and draws the card instead. A title
// written again later (a status dot's label changes) is picked up the same
// way. The element keeps its accessible name: an icon-only button without an
// `aria-label` gets the title as one.
//
// Timing and look follow `Blockr.tooltip` in blockr.ui: shown after the
// pointer rests 300ms, at once on keyboard focus and while "warm" (within
// 400ms of another card leaving), above the element and below only where
// there is no room, gone with the pointer, focus, Escape, a press or a
// scroll. Once blockr.ui ships `Blockr.tooltip`, the sites move to it and
// this file goes.
(function () {
  var SCOPE = '.blockr-block-header, .blockr-navbar, .blockr-view-nav, .blockr-sidebar';
  var DELAY = 300;
  var WARM = 400;
  var GAP = 6;
  var MARGIN = 8;

  var card = null;
  var current = null;
  var timer = null;
  var warmUntil = 0;

  // The nearest element with a tooltip inside the dock's chrome, its title
  // moved over first so the native box stays away.
  function target(node) {
    var el = node instanceof Element ? node : null;
    while (el && !el.hasAttribute('title') && !el.hasAttribute('data-blockr-tip')) {
      el = el.parentElement;
    }
    if (!el || !el.closest(SCOPE)) return null;
    var title = el.getAttribute('title');
    if (title !== null) {
      el.removeAttribute('title');
      if (title) {
        el.setAttribute('data-blockr-tip', title);
        if (!el.hasAttribute('aria-label') && !el.textContent.trim()) {
          el.setAttribute('aria-label', title);
        }
      } else {
        el.removeAttribute('data-blockr-tip');
      }
    }
    return el.getAttribute('data-blockr-tip') ? el : null;
  }

  function hide() {
    if (timer) { clearTimeout(timer); timer = null; }
    if (card && card.isConnected) {
      card.remove();
      warmUntil = Date.now() + WARM;
    }
    if (current) current.removeAttribute('aria-describedby');
    current = null;
  }

  function show(el) {
    timer = null;
    var text = el.isConnected ? el.getAttribute('data-blockr-tip') : null;
    if (!text) return;
    if (!card) {
      card = document.createElement('div');
      card.className = 'blockr-tooltip';
      card.id = 'blockr-dock-tooltip';
      card.setAttribute('role', 'tooltip');
    }
    card.textContent = text;
    document.body.appendChild(card);
    var r = el.getBoundingClientRect();
    var c = card.getBoundingClientRect();
    var left = Math.max(MARGIN, Math.min(r.left + r.width / 2 - c.width / 2,
      window.innerWidth - c.width - MARGIN));
    var top = r.top - c.height - GAP;
    if (top < MARGIN) top = r.bottom + GAP;
    card.style.left = left + 'px';
    card.style.top = top + 'px';
    el.setAttribute('aria-describedby', card.id);
    current = el;
  }

  function enter(e) {
    var el = target(e.target);
    if (!el || el === current) return;
    hide();
    if (e.type === 'focusin' || Date.now() < warmUntil) show(el);
    else timer = setTimeout(function () { show(el); }, DELAY);
  }

  function leave(e) {
    if (!current && !timer) return;
    var el = e.target instanceof Element ? e.target.closest('[data-blockr-tip]') : null;
    if (!el) return;
    if (e.relatedTarget instanceof Node && el.contains(e.relatedTarget)) return;
    hide();
  }

  document.addEventListener('pointerover', enter, true);
  document.addEventListener('focusin', enter, true);
  document.addEventListener('pointerout', leave, true);
  document.addEventListener('focusout', hide, true);
  document.addEventListener('pointerdown', hide, true);
  document.addEventListener('scroll', hide, true);
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') hide();
  }, true);
})();
