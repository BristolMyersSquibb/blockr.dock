// The navbar logo's tooltip (logo_navbar_ui() in R): "Computing" while the
// board is busy, and none while it is not. Busy is the scope the logo's
// animation runs on in blockr-dock.css, read when the tooltip is about to
// show. A logo is taken up on its first hover, in the capture phase on
// `window`, ahead of Blockr.tooltip's listeners on `document`, so a board drawn
// at any time needs nothing more. A tooltip on screen when the work ends goes
// with it, as the animation does, rather than say "Computing" over a logo at
// rest.
(function () {
  'use strict';

  var BUSY = '.shiny-busy:has(.blockr-view-container .recalculating)';
  var taken = new WeakSet();

  function label() {
    return document.documentElement.matches(BUSY) ? 'Computing' : null;
  }

  window.addEventListener('pointerover', function (e) {
    var el = e.target instanceof Element ?
      e.target.closest('.blockr-navbar-logo') : null;
    if (!el || taken.has(el)) return;
    taken.add(el);
    Blockr.tooltip.set(el, label);
  }, true);

  $(document).on('shiny:idle', function () {
    document.querySelectorAll('.blockr-navbar-logo').forEach(function (el) {
      if (!taken.has(el)) return;
      Blockr.tooltip.clear(el);
      Blockr.tooltip.set(el, label);
    });
  });
})();
