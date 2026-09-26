// The "Compact" board option: `.blockr-compact` on the root switches every
// block header to its eyebrow form (blockr-dock.css).
(function () {
  'use strict';
  Shiny.addCustomMessageHandler('blockr-compact', function (on) {
    document.documentElement.classList.toggle('blockr-compact', on === true);
  });
})();
