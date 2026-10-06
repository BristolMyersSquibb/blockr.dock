// A bridge until the dock requires an htmlwidgets release with
// ramnathv/htmlwidgets#496 (#528). Up to htmlwidgets 1.6.4, a static widget,
// one that a block or an extension builds in its UI rather than through a
// render function, re-measures only on a window resize or a `shown` event.
// The dock resizes widgets with neither: a card is built off screen and moved
// into its panel, a section folds open, a splitter is dragged. This watches
// each static widget's size, as the upstream fix does, and on a change fires
// the event htmlwidgets listens for. Shiny's outputs need none of it: Shiny
// 1.14 watches them itself.
(function () {
  'use strict';

  if (typeof ResizeObserver === 'undefined') return;

  var watched = new WeakSet();

  // Each static widget compares its size with the one it last saw, so one
  // event serves every widget that changed.
  var observer = new ResizeObserver(function () {
    $(document).trigger('shown.htmlwidgets');
  });

  function watch() {
    document.querySelectorAll('.html-widget-static-bound').forEach(function (el) {
      if (!watched.has(el)) {
        watched.add(el);
        observer.observe(el);
      }
    });
  }

  // A post-render handler runs once, after the next static render, and one
  // added while the handlers run would run straight away: re-arm on the next
  // turn instead.
  function rendered() {
    watch();
    setTimeout(arm, 0);
  }

  function arm() {
    window.HTMLWidgets.addPostRenderHandler(rendered);
  }

  function start() {
    if (!window.HTMLWidgets) return;
    watch();
    arm();
  }

  if (window.HTMLWidgets) {
    start();
  } else {
    document.addEventListener('DOMContentLoaded', start);
  }
})();
