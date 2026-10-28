// The navbar logo (logo_navbar_ui() in R) as the board's busy indicator. The
// logo carries `is-computing` while Shiny is busy and an output in the view
// container is recalculating; blockr-dock.css animates it on that class, and
// its tooltip then says "Computing". A `:has()` in the stylesheet used to read
// the same scope, at the cost of restyling the whole page on every output
// update (see blockr-dock.css). Here it is read once a frame at most, after the
// Shiny events that change it: busy and idle flip `.shiny-busy`, and an output
// turns `.recalculating` when it is invalidated or bound mid-flush and stops
// when its value or error arrives. Any other change is caught by the next of
// those, idle at the latest.
//
// The tooltip is taken up on the logo's first hover, in the capture phase on
// `window`, ahead of Blockr.tooltip's listeners on `document`, so a board
// drawn at any time needs nothing more. A tooltip on screen when the work ends
// goes with it, as the animation does, rather than say "Computing" over a logo
// at rest.
(function () {
  'use strict';

  var LOGO = '.blockr-navbar-logo';
  var CLASS = 'is-computing';
  var taken = new WeakSet();
  var queued = false;

  function label(el) {
    return function () {
      return el.classList.contains(CLASS) ? 'Computing' : null;
    };
  }

  function update() {
    queued = false;
    var on = document.documentElement.classList.contains('shiny-busy') &&
      document.querySelector('.blockr-view-container .recalculating') !== null;
    document.querySelectorAll(LOGO).forEach(function (el) {
      if (el.classList.contains(CLASS) === on) return;
      el.classList.toggle(CLASS, on);
      if (!on && taken.has(el)) {
        Blockr.tooltip.clear(el);
        Blockr.tooltip.set(el, label(el));
      }
    });
  }

  function schedule() {
    if (queued) return;
    queued = true;
    requestAnimationFrame(update);
  }

  window.addEventListener('pointerover', function (e) {
    var el = e.target instanceof Element ? e.target.closest(LOGO) : null;
    if (!el || taken.has(el)) return;
    taken.add(el);
    Blockr.tooltip.set(el, label(el));
  }, true);

  $(document).on(
    'shiny:busy shiny:idle shiny:outputinvalidated shiny:bound ' +
      'shiny:value shiny:error',
    schedule
  );
})();
